package bootstrap.rinamo

import scala.tools.nsc.MainGenericRunner

import com.amazonaws._
import com.amazonaws.auth._
import com.amazonaws.services.dynamodbv2._
import com.amazonaws.services.dynamodbv2.model._

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

   def config():AmazonDynamoDBClient = {
     val input_stream = classOf[DynamoDBTest]
                          .getResourceAsStream("AwsCredentials.properties")

     input_stream.mark(Integer.MAX_VALUE)
     val props = new Properties()
     props.load(input_stream)
     val protocol = DynamoDBTest.getConfigValue("protocol", props)
     val host = DynamoDBTest.getConfigValue("host", props)
     val port = DynamoDBTest.getConfigValue("port", props)
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
       client.setEndpoint(protocol + "://" + host + ":" + port)
     }

     // Control Output
     val cout = new ConsoleRedirector("trace")
     val cerr = new ConsoleRedirector("error")
     System.setOut(cout); System.setErr(cerr)
     Console.setOut(cout); Console.setErr(cerr)
     
     return client
   }
}