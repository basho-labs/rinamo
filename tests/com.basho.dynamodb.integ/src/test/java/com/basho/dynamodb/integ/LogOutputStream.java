/* ---------------------------------------------------------------------
%%
%% Copyright (c) 2007-2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% ---------------------------------------------------------------------*/

package com.basho.dynamodb.integ;

import java.io.IOException;
import java.io.OutputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * An output stream which writes to the application logger.
 * This is used to redirect rouge console output into the
 * logger architecture.
 *
 * Created on Oct 27, 2005 at 12:20:34 PM
 * @author Randy Secrist
 */
public class LogOutputStream extends OutputStream {

    /**
     * The buffer where data is stored.
     */
    protected byte buf[];

    /**
     * The logger where the buffer is written.
     */
    protected Logger log = LoggerFactory.getLogger(LogOutputStream.class);

    /**
     * The logger level to use when writing to the log.
     * @param level The logger level to use when writing to the log.
     */
    protected String level;

    LogOutputStream(String level) {
        this.level = level;
    }

    /**
     * Writes the specified byte to this byte array output stream.
     *
     * @param   b   the byte to be written.
     */
    public synchronized void write(int b) {
        buf = new byte[1];
        buf[0] = (byte) b;
        flush();
    }

    /**
     * Writes <code>b.length</code> bytes from the specified byte array
     * to the logger api.
     *
     * @param      b   the data.
     */
    public synchronized void write(byte[] b) {
        buf = b;
        flush();
    }

    /**
     * Writes <code>len</code> bytes from the specified byte array
     * starting at offset <code>off</code> to the logger api.
     *
     * @param   b     the data.
     * @param   off   the start offset in the data.
     * @param   len   the number of bytes to write.
     */
    public synchronized void write(byte[] b, int off, int len) {
        if ((off < 0) || (off > b.length) || (len < 0) || ((off + len) > b.length) || ((off + len) < 0)) {
            throw new IndexOutOfBoundsException();
        }
        else if (len == 0) {
            return;
        }
        else if (len == 1 && b[0] == '\n') {
        	return;
        }
        else if (len == 2 && b[0] == '\r' && b[1] == '\n') {
        	return;
        }
        buf = new byte[len];
        System.arraycopy(b, off, buf, 0, len);
        flush();
    }

    /**
     * Closing a <tt>LogOutputStream</tt> has no effect. The methods in
     * this class can be called after the stream has been closed without
     * generating an <tt>IOException</tt>.
     * <p>
     *
     */
    public void close() throws IOException {
    }

    /**
     * Writes the internal byte array to the logger api.
     * Note, the only way to set the byte array is to call
     * one of the <quote>write</quote> functions of this class.
     */
    public synchronized void flush() {
    	/* Used to detect components that do something stupid with logging.
    	try {
    		throw new RuntimeException("HERE");
    	}
    	catch (RuntimeException e) {
    		StringWriter sw = new StringWriter();
    		e.printStackTrace(new PrintWriter(sw));
    		String stacktrace = sw.toString();
    		log.fatal(stacktrace);
    	}
    	*/
        if (buf != null && buf.length > 0) {
            if ("trace".equalsIgnoreCase(level)) {
                log.trace(new String(buf));
            }
            else if ("debug".equalsIgnoreCase(level)) {
                log.debug(new String(buf));
            }
            else if ("info".equalsIgnoreCase(level)) {
                log.info(new String(buf));
            }
            else if ("warn".equalsIgnoreCase(level)) {
                log.warn(new String(buf));
            }
            else if ("error".equalsIgnoreCase(level)) {
                log.error(new String(buf));
            }
            else {
                log.trace(new String(buf));
            }
            buf = null;
        }
    }
}
