package com.basho.dynamodb.integ;

import java.io.PrintStream;

public class ConsoleRedirector extends PrintStream {
    public ConsoleRedirector(String level) {
        super(new LogOutputStream(level));
    }
}