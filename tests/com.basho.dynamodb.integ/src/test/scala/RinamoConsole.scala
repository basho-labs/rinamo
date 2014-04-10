import bootstrap.rinamo.Boot
import scala.tools.nsc.MainGenericRunner

import com.amazonaws.auth._
import com.amazonaws.services.dynamodbv2._
import com.amazonaws.services.dynamodbv2.model._

import java.util.Properties

object RinamoConsole {
   def main(args : Array[String]) {
     val b = new Boot()
     b.boot
     MainGenericRunner.main(args)
     exit(0)
   }
   
   def config():AmazonDynamoDBClient = {
     val input_stream = classOf[com.basho.dynamodb.integ.DynamoDBTest]
                          .getResourceAsStream("AwsCredentials.properties")

     input_stream.mark(Integer.MAX_VALUE)
     val props = new Properties()
     props.load(input_stream)
     val protocol = props.getProperty("protocol")
     val host = props.getProperty("host")
     val port = props.getProperty("port")
     input_stream.reset()

     val creds = new PropertiesCredentials(input_stream)
     val client = new AmazonDynamoDBClient(creds)
     
     if (host != null && host.length() > 0) {
       client.setEndpoint(protocol + "://" + host + ":" + port)
     }
     return client
   }
}