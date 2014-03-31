import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

import com.amazonaws.auth._
import com.amazonaws.services.dynamodbv2._
import com.amazonaws.services.dynamodbv2.model._

object Table {
  val creds = new PropertiesCredentials(classOf[com.basho.dynamodb.integ.DynamoDBTest].getResourceAsStream("AwsCredentials.properties"))
  val client = new AmazonDynamoDBClient(creds)
  client.setEndpoint("http://localhost:8000")

  def create(table_name:String, key_name:String, key_type:String = "N", rc:Long = 1, wc:Long = 1): CreateTableResult = {
    val key_schema_list = List(
        new KeySchemaElement().withAttributeName(key_name).withKeyType(KeyType.HASH)
    )
    val attribute_defs = List(
        new AttributeDefinition().withAttributeName(key_name).withAttributeType(key_type)
    )
    
    val provisioned_throughput = new ProvisionedThroughput()
                                       .withReadCapacityUnits(rc)
                                       .withWriteCapacityUnits(wc);
    
    val request = new CreateTableRequest()
      .withTableName(table_name)
      .withKeySchema(key_schema_list.asJava)
      .withAttributeDefinitions(attribute_defs.asJava)
      .withProvisionedThroughput(provisioned_throughput)
      
    val result = client.createTable(request)
    return result
  }

  def list(): List[String] = {
    return client.listTables().getTableNames().toList
  }
  
  def describe(table_name:String): DescribeTableResult = {
    return client.describeTable(table_name)
  }
  
  def delete(table_name:String) {
    client.deleteTable(table_name)
  }

}