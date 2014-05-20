package acceptance

import scala.collection.JavaConversions._
import scala.collection.mutable.MutableList
import com.amazonaws.services.dynamodbv2.model.AttributeDefinition
import com.amazonaws.services.dynamodbv2.model.KeySchemaElement

class Attributes(attributes:(String, String)*) {
  private var list: List[AttributeDefinition] = List()

  for (attribute <- attributes) {
    attribute match {
      case (_name:String, _type:String) =>
        add(_name, _type)
      case _ => None
    }
  }

  def add(_name:String, _type:String) {
    list :::= List(new AttributeDefinition().withAttributeName(_name).withAttributeType(_type))
  }

  override def toString: String = {
    list.toString
  }

  def asList(): List[AttributeDefinition] = {
    return list
  }

  def asCollection(): java.util.Collection[AttributeDefinition] = {
    return asJavaCollection(list)
  }
}

class KeySchema {
  val HASH = KeyType.HASH
  val RANGE = KeyType.RANGE
  private var list: List[KeySchemaElement] = List()
  
  def add(_name:String, _type:com.amazonaws.services.dynamodbv2.model.KeyType) = {
    list :::= List(new KeySchemaElement().withAttributeName(_name).withKeyType(_type))
    // AWS requires HASH come before RANGE or it yells & I don't want to have to worry
    // about this in each test case
    list = list.sortBy(_.getKeyType)
  }
  
  override def toString: String = {
    list.toString
  }

  def asList(): List[KeySchemaElement] = {
    return list
  }

  def asCollection(): java.util.Collection[KeySchemaElement] = {
    return asJavaCollection(list)
  }
}

class LocalSecondaryIndexes(
    indexes:(
      String,
      KeySchema,
      com.amazonaws.services.dynamodbv2.model.Projection)*) {
  var list: List[com.amazonaws.services.dynamodbv2.model.LocalSecondaryIndex] = List()
  for (index <- indexes) {
    index match {
      case (_index_name, _key_schema, _projection) =>
        add(_index_name, _key_schema, _projection)
      case _ => None
    }
  }

  def add(
      _index_name:String, _key_schema:KeySchema,
      _projection:com.amazonaws.services.dynamodbv2.model.Projection) {

    list :::=  List(new com.amazonaws.services.dynamodbv2.model.LocalSecondaryIndex()
      .withIndexName(_index_name).withKeySchema(_key_schema.asCollection)
      .withProjection(_projection))
  }

  override def toString: String = {
    list.toString
  }

  def asList(): List[com.amazonaws.services.dynamodbv2.model.LocalSecondaryIndex] = {
    return list
  }

  def asCollection(): java.util.Collection[com.amazonaws.services.dynamodbv2.model.LocalSecondaryIndex] = {
    return asJavaCollection(list)
  }
}

object ProvisionedThroughput {
  def build_value(_rc:Long, _wc:Long): com.amazonaws.services.dynamodbv2.model.ProvisionedThroughput = {
    return new com.amazonaws.services.dynamodbv2.model.ProvisionedThroughput(_rc, _wc)
  }
}

object Projection {
  def build_value(_type:com.amazonaws.services.dynamodbv2.model.ProjectionType, _name_list: List[String]): com.amazonaws.services.dynamodbv2.model.Projection = {
    return new com.amazonaws.services.dynamodbv2.model.Projection().
      withProjectionType(_type).
      withNonKeyAttributes(asJavaCollection(_name_list))
  }
}

object ProjectionType {
  val ALL = com.amazonaws.services.dynamodbv2.model.ProjectionType.ALL
  val INCLUDE = com.amazonaws.services.dynamodbv2.model.ProjectionType.INCLUDE
  val KEYS_ONLY = com.amazonaws.services.dynamodbv2.model.ProjectionType.KEYS_ONLY
}

object KeyType {
  val HASH = com.amazonaws.services.dynamodbv2.model.KeyType.HASH
  val RANGE = com.amazonaws.services.dynamodbv2.model.KeyType.RANGE
}