package acceptance

import scala.collection.JavaConversions._
import scala.collection.mutable.MutableList
import com.amazonaws.services.dynamodbv2.model.KeySchemaElement
import com.amazonaws.services.dynamodbv2.model.AttributeDefinition

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
  }
  
  def asList(): List[KeySchemaElement] = {
    return list
  }

  def asCollection(): java.util.Collection[KeySchemaElement] = {
    return asJavaCollection(list)
  }
}

object LocalSecondaryIndex {
  def build_list(
    indexes:(
      String,
      List[com.amazonaws.services.dynamodbv2.model.KeySchemaElement],
      com.amazonaws.services.dynamodbv2.model.Projection)*)
    : java.util.Collection[com.amazonaws.services.dynamodbv2.model.LocalSecondaryIndex] = {

    val lsi_list: MutableList[com.amazonaws.services.dynamodbv2.model.LocalSecondaryIndex] = MutableList()
    for (index <- indexes) {
      index match {
        case (_index_name, _key_schema, _projection) =>
          lsi_list +=  new com.amazonaws.services.dynamodbv2.model.LocalSecondaryIndex()
            .withIndexName(_index_name).withKeySchema(asJavaCollection(_key_schema))
            .withProjection(_projection)
        case _ => None
      }
    }
    return asJavaCollection(lsi_list)
  }
}

object Projection {
  def build_value(_name_list: List[String]): com.amazonaws.services.dynamodbv2.model.Projection = {
    return new com.amazonaws.services.dynamodbv2.model.Projection().
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