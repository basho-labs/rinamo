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

import scala.collection.JavaConverters._
import scala.collection.JavaConversions._
import com.amazonaws.auth._
import com.amazonaws.services.dynamodbv2._
import com.amazonaws.services.dynamodbv2.model._
import com.amazonaws.services.dynamodbv2.datamodeling._

import bootstrap.rinamo.RinamoConsole

object Table {
  var client = RinamoConsole.dynamo_config()
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

  def create(
      table_name:String,
      key_schema:KeySchema,
      attributes:Attributes,
      lsi_secondary_indexes:Option[LocalSecondaryIndexes],
      gsi_secondary_indexes:Option[GlobalSecondaryIndexes],
      provisioned_throughput:ProvisionedThroughput): CreateTableResult = {

    val request = new com.amazonaws.services.dynamodbv2.model.CreateTableRequest().
      withTableName(table_name).
      withKeySchema(key_schema.asCollection).
      withAttributeDefinitions(attributes.asCollection).
      withProvisionedThroughput(provisioned_throughput)

    lsi_secondary_indexes match {
      case Some(lsi_secondary_indexes) => {
        request.setLocalSecondaryIndexes(lsi_secondary_indexes.asCollection)
      }
      case None => {}
    }
    gsi_secondary_indexes match {
      case Some(gsi_secondary_indexes) => {
        request.setGlobalSecondaryIndexes(gsi_secondary_indexes.asCollection)
      }
      case None => {}
    }

    return client.createTable(request)
  }

  def list(): List[String] = {
    return client.listTables().getTableNames().toList
  }

  def describe(table_name:String): DescribeTableResult = {
    return client.describeTable(table_name)
  }

  /* Delete a Table */
  def delete(table_name:String): DeleteTableResult = {
    return client.deleteTable(table_name)
  }

  def put(table_name:String, item:Item,
          _expected:Option[Map[String, ExpectedAttributeValue]] = None,
          _returnValue:Option[ReturnValue] = Some(ReturnValue.NONE)): PutItemResult = {
    val request = new PutItemRequest().withItem(item.asMap())
    request.setTableName(table_name)
    _returnValue match {
      case Some(_returnValue) => { request.setReturnValues(_returnValue) }
      case None => { request.setReturnValues(ReturnValue.NONE) }
    }
    _expected match {
      case Some(_expected) => { request.setExpected(_expected) }
      case None => {}
    }
    return client.putItem(request)
  }

  def put(table_name:String)(items:Item*) {
    for (item <- items) {
      put(table_name, item)
    }
  }

  def get(
      table_name:String,
      _key:String, _value:String, _type:String): GetItemResult = {

    val (_, value) = Item.build_value(_key, _type, _value)
    val request = new GetItemRequest().addKeyEntry(_key, value)
    request.setTableName(table_name)
    return client.getItem(request)
  }

  def delete(table_name:String,
             _hash_key:String, _hash_value:String, _hash_type:String,
             _range_key:Option[String] = None, _range_value:Option[String] = None,
             _range_type:Option[String] = Some("N")): DeleteItemResult = {
    val (_, h_value) = Item.build_value(_hash_key, _hash_type, _hash_value)
    val request = new DeleteItemRequest().addKeyEntry(_hash_key, h_value)
    _range_key match {
      case Some(_range_key) => {
        val (_, r_value) = Item.build_value(_range_key, _range_type.getOrElse("N"), _range_value.get)
        request.addKeyEntry(_range_key, r_value)
        (_range_key, r_value)
      }
      case None => {}
    }
    request.setTableName(table_name)
    return client.deleteItem(request)
  }

  /*
   * Range Query Convenience Function
   * Assumes the hash key is a number, range key is a string.
   * Use 'query' if you want full control over this.
   */
  def range_query(table_name:String,
      hash_key:String, hash_value:String,
      range_key:Option[String] = None,
      operator:Option[String] = Some(ComparisonOperator.EQ.toString()),
      range_value:Option[String] = None,
      between_value:Option[String] = None): QueryResult = {
    val key_conditions = new KeyConditions
    key_conditions.add(hash_key, "N", "EQ", hash_value)
    range_key match {
      case Some(range_key) => {
        key_conditions.add(range_key, "S", operator.get, range_value.get, between_value)
      }
      case None => {}
    }
    return query(table_name, key_conditions)
  }

  def query(
      table_name:String, key_conditions:KeyConditions,
      index_name:Option[String] = None): QueryResult = {
    val request = new QueryRequest()
    request.setTableName(table_name)
    request.setKeyConditions(key_conditions.asMap)
    index_name match {
      case Some(index_name) => { request.setIndexName(index_name) }
      case None => {}
    }
    // request.setAttributesToGet(attributesToGet)
    // request.setConsistentRead(consistentRead)
    return client.query(request)
  }

  def scan(table_name:String): ScanResult = {
    val request = new ScanRequest()
    request.setTableName(table_name)
    client.scan(request)
  }

  def reset() {
    client = RinamoConsole.dynamo_config()
  }
}
