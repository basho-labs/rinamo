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

package acceptance

import scala.collection.JavaConversions._
import scala.collection.mutable.MutableList
import com.amazonaws.services.dynamodbv2.model.AttributeDefinition
import com.amazonaws.services.dynamodbv2.model.AttributeValue
import com.amazonaws.services.dynamodbv2.model.KeySchemaElement
import com.amazonaws.services.dynamodbv2.model.ComparisonOperator

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

class KeyConditions(conditions:(String, String, String, String, Option[String])*) {
  private var map: Map[String, com.amazonaws.services.dynamodbv2.model.Condition] = Map()
  for (condition <- conditions) {
    condition match {
      case (_key:String, _attr_val_type:String, _operator:String, _attr_val:String, between_value:Option[String]) =>
        add(_key, _attr_val_type, _operator, _attr_val, between_value)
      case _ => None
    }
  }

  def add(_key:String, _attr_val_type:String, _operator:String, _attr_val:String,
      between_value:Option[String] = None) {
    map += ((_key, Condition.build_value(_operator, _attr_val_type, _attr_val, between_value)))
  }

  override def toString: String = {
    map.toString
  }

  def asMap(): Map[String, com.amazonaws.services.dynamodbv2.model.Condition] = {
    return map
  }
}

class KeySchema(elements:(String, com.amazonaws.services.dynamodbv2.model.KeyType)*) {
  val HASH = KeyType.HASH
  val RANGE = KeyType.RANGE
  private var list: List[KeySchemaElement] = List()
  for (element <- elements) {
    element match {
      case (_name:String, _type:com.amazonaws.services.dynamodbv2.model.KeyType) =>
        add(_name, _type)
      case _ => None
    }
  }

  def add(_name:String, _type:com.amazonaws.services.dynamodbv2.model.KeyType) = {
    list :::= List(new KeySchemaElement().withAttributeName(_name).withKeyType(_type))
    // Don't make test cases figure out key schema element ordering
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

class GlobalSecondaryIndexes(
    indexes:(
      String,
      KeySchema,
      com.amazonaws.services.dynamodbv2.model.Projection,
      com.amazonaws.services.dynamodbv2.model.ProvisionedThroughput)*) {
  var list: List[com.amazonaws.services.dynamodbv2.model.GlobalSecondaryIndex] = List()
  for (index <- indexes) {
    index match {
      case (_index_name, _key_schema, _projection, _provisioned_throughput) =>
        add(_index_name, _key_schema, _projection, _provisioned_throughput)
      case _ => None
    }
  }

  def add(
      _index_name:String, _key_schema:KeySchema,
      _projection:com.amazonaws.services.dynamodbv2.model.Projection,
      _provisioned_throughput:com.amazonaws.services.dynamodbv2.model.ProvisionedThroughput) {

    list :::=  List(new com.amazonaws.services.dynamodbv2.model.GlobalSecondaryIndex()
      .withIndexName(_index_name).withKeySchema(_key_schema.asCollection)
      .withProjection(_projection)
      .withProvisionedThroughput(_provisioned_throughput))
  }

  override def toString: String = {
    list.toString
  }

  def asList(): List[com.amazonaws.services.dynamodbv2.model.GlobalSecondaryIndex] = {
    return list
  }

  def asCollection(): java.util.Collection[com.amazonaws.services.dynamodbv2.model.GlobalSecondaryIndex] = {
    return asJavaCollection(list)
  }
}

object Condition {
  def build_value(_operator:String, _attr_val_type:String, _attr_val:String,
      between_value:Option[String] = None): com.amazonaws.services.dynamodbv2.model.Condition = {
    val op = com.amazonaws.services.dynamodbv2.model.ComparisonOperator.fromValue(_operator)
    return op match {
      case ComparisonOperator.BETWEEN => {
        val begin_val = _attr_val
        val end_val = between_value.get
        val attr_val_list: List[AttributeValue] = _attr_val_type.toLowerCase() match {
          case "n" => List(
            new AttributeValue().withN(begin_val),
            new AttributeValue().withN(end_val)
          )
          case "s" => List(
            new AttributeValue().withS(begin_val),
            new AttributeValue().withS(end_val)
          )
          case _ => null
        }
        new com.amazonaws.services.dynamodbv2.model.Condition()
          .withComparisonOperator(op)
          .withAttributeValueList(attr_val_list)
      }
      case _ => {
        val condition_attr = new AttributeValue()
        _attr_val_type.toLowerCase() match {
          case "n" => condition_attr.setN(_attr_val)
          case "s" => condition_attr.setS(_attr_val)
          case _ => None
        }
        new com.amazonaws.services.dynamodbv2.model.Condition()
          .withComparisonOperator(op)
          .withAttributeValueList(condition_attr)
      }
    }
  }
}

object ProvisionedThroughput {
  def build_value(_rc:Long, _wc:Long): com.amazonaws.services.dynamodbv2.model.ProvisionedThroughput = {
    return new com.amazonaws.services.dynamodbv2.model.ProvisionedThroughput(_rc, _wc)
  }
}

object Projection {
  def build_value(_type:com.amazonaws.services.dynamodbv2.model.ProjectionType, _name_list: Option[List[String]] = None): com.amazonaws.services.dynamodbv2.model.Projection = {
    _name_list match {
      case Some(_name_list) => {
        return new com.amazonaws.services.dynamodbv2.model.Projection().
          withProjectionType(_type).
          withNonKeyAttributes(asJavaCollection(_name_list))
      }
      case _ => {
        return new com.amazonaws.services.dynamodbv2.model.Projection().
          withProjectionType(_type)
      }
    }
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

class Item(attributes:(String, String, String)*) {
  private var map: Map[String, AttributeValue] = Map()
  for (attribute <- attributes) {
    attribute match {
      case (_name:String, _type:String, _value:String) =>
        add(_name, _type, _value)
      case _ => None
    }
  }

  def add(_name:String, _type:String, _value:String): Item = {
    val value = Item.build_value(_name, _type, _value)
    map += value
    return Item.this
  }

  override def toString: String = {
    map.toString
  }

  def asMap():Map[String, AttributeValue] = {
    return map
  }
}

object Item {
  def build_value(_name:String, _type:String, _value:String):(String, AttributeValue) = {
    return _type match {
      case "N" => (_name, new AttributeValue().withN(_value))
      case "S" => (_name, new AttributeValue().withS(_value))
    }
  }
}
