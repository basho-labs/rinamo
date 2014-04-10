import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

import com.amazonaws.auth._
import com.amazonaws.services.dynamodbv2._
import com.amazonaws.services.dynamodbv2.model._
import com.amazonaws.services.dynamodbv2.datamodeling._

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

    return client.createTable(request)
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
    
  def put(table_name:String, item:Item): PutItemResult = {
    val request = new PutItemRequest().withItem(item.asMap())
    request.setTableName(table_name)
    return client.putItem(request)
  }

  def get(
      table_name:String,
      _key:String, _type:String, _value:String): GetItemResult = {

    val (_, value) = Item.build_value(_key, _type, _value)
    val request = new GetItemRequest().addKeyEntry(_key, value)
    request.setTableName(table_name)
    return client.getItem(request)
  }

  def delete(
      table_name:String,
      _key:String, _type:String, _value:String): DeleteItemResult = {

    val (_, value) = Item.build_value(_key, _type, _value)
    val request = new DeleteItemRequest().addKeyEntry(_key, value)
    request.setTableName(table_name)
    return client.deleteItem(request)
  }

  def query(table_name:String, range_key:String, operator:String, range_value:String):QueryResult = {
    val request = new QueryRequest()

    val hash_condition = new Condition()
      .withComparisonOperator(ComparisonOperator.EQ.toString())
      .withAttributeValueList(new AttributeValue().withN(range_key))

    // any string val > "c"
    val range_condition = new Condition()
      .withComparisonOperator(ComparisonOperator.fromValue(operator))
      .withAttributeValueList(new AttributeValue().withS(range_value))

    var key_conditions:Map[String, Condition] = Map()
    key_conditions += (("Id", hash_condition))
    key_conditions += (("Title", range_condition))

    request.setKeyConditions(key_conditions)
    request.setTableName(table_name)

    return client.query(request)
  }

}