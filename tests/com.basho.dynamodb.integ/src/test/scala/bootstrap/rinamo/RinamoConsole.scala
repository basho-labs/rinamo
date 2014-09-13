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

package bootstrap.rinamo

import scala.tools.nsc.MainGenericRunner
import com.amazonaws._
import com.amazonaws.auth._
import com.amazonaws.services.dynamodbv2._
import com.amazonaws.services.dynamodbv2.model._
import com.amazonaws.services.s3._
import java.util.Properties
import com.basho.dynamodb.integ.DynamoDBTest
import com.basho.dynamodb.integ.ConsoleRedirector

object RinamoConsole {
   def main(args : Array[String]) {
     val b = new Boot()
     b.boot
     MainGenericRunner.main(args)
     System.exit(0)
   }

   def s3_config():AmazonS3Client = {
     val input_stream = classOf[DynamoDBTest]
                          .getResourceAsStream("AwsCredentials.properties")
     val creds = new PropertiesCredentials(input_stream)
     val config = new ClientConfiguration()
     val client = new AmazonS3Client(creds, config)
     return client
   }

   def dynamo_config(): AmazonDynamoDBClient = {
     val input_stream = classOf[DynamoDBTest]
                          .getResourceAsStream("AwsCredentials.properties")

     input_stream.mark(Integer.MAX_VALUE)
     val props = new Properties()
     props.load(input_stream)
     val protocol = DynamoDBTest.getConfigValue("protocol", props)
     val host = DynamoDBTest.getConfigValue("host", props)
     val port = DynamoDBTest.getConfigValue("port", props)
     val root_path = DynamoDBTest.getConfigValue("root_path", props)
     input_stream.reset()

     val creds = new PropertiesCredentials(input_stream)
     val config = new ClientConfiguration()
     config.setMaxErrorRetry(0)
     config.setSocketTimeout(3 * 1000)
     config.setConnectionTimeout(1000)
     config.setMaxConnections(10)
     config.setUserAgent("Rinamo Console")
     val client = new AmazonDynamoDBClient(creds, config)

     if (host != null && host.length() > 0) {
       client.setEndpoint(protocol + "://" + host + ":" + port + root_path)
     }

     // Control Output
     val cout = new ConsoleRedirector("trace")
     val cerr = new ConsoleRedirector("error")
     System.setOut(cout); System.setErr(cerr)
     Console.setOut(cout); Console.setErr(cerr)

     return client
   }
}
