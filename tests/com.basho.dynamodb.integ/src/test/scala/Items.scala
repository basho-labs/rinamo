import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

import com.amazonaws.auth._
import com.amazonaws.services.dynamodbv2._
import com.amazonaws.services.dynamodbv2.model._

object Items {
  val creds = new PropertiesCredentials(classOf[com.basho.dynamodb.integ.DynamoDBTest].getResourceAsStream("AwsCredentials.properties"))
  val client = new AmazonDynamoDBClient(creds)
  client.setEndpoint("http://localhost:8000")

  def get() {
  }
  
  def put() {
  }
  
  def delete() {
  }
}