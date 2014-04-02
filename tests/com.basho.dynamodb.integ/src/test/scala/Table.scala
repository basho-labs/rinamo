import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

import com.amazonaws.auth._
import com.amazonaws.services.dynamodbv2._
import com.amazonaws.services.dynamodbv2.model._

object Table {
  val client = RinamoConsole.config()
  def create(
      table_name:String, key_name:String, key_type:String = "N",
      range_key_name:Option[String] = None,
      range_key_type:Option[String] = Some("N"),
      rc:Long = 1, wc:Long = 1): CreateTableResult = {

    val (key_schema_list, attribute_defs) = range_key_name match {
      case Some(range_key_name) => {(
        List(
          new KeySchemaElement(key_name, KeyType.HASH),
          new KeySchemaElement(range_key_name, KeyType.RANGE)
        ),
        List(
          new AttributeDefinition(key_name, key_type),
          new AttributeDefinition(range_key_name, range_key_type.getOrElse("N"))
        )
      )}
      case None => {(
        List(new KeySchemaElement(key_name, KeyType.HASH)),
        List(new AttributeDefinition(key_name, key_type))
      )}
    }

    val provisioned_throughput = new ProvisionedThroughput(rc, wc)

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

  def delete(table_name:String): DeleteTableResult = {
    return client.deleteTable(table_name)
  }

}