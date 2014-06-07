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

class Attributes(attributes:(String, String)*) extends acceptance.Attributes {
  for (attribute <- attributes) {
    attribute match {
      case (_name:String, _type:String) =>
        add(_name, _type)
      case _ => None
    }
  }
}
class Item(attributes:(String, String, String)*) extends acceptance.Item {
  for (attribute <- attributes) {
    attribute match {
      case (_name:String, _type:String, _value:String) =>
        add(_name, _type, _value)
      case _ => None
    }
  }
}
class KeyConditions(conditions:(String, String, String, String, Option[String])*) extends acceptance.KeyConditions {
  for (condition <- conditions) {
    condition match {
      case (_key:String, _attr_val_type:String, _operator:String, _attr_val:String, between_value:Option[String]) =>
        add(_key, _attr_val_type, _operator, _attr_val, between_value)
      case _ => None
    }
  }
}
class KeySchema(elements:(String, com.amazonaws.services.dynamodbv2.model.KeyType)*) extends acceptance.KeySchema {
  for (element <- elements) {
    element match {
      case (_name:String, _type:com.amazonaws.services.dynamodbv2.model.KeyType) =>
        add(_name, _type)
      case _ => None
    }
  }
}
class LocalSecondaryIndexes(indexes:(String, KeySchema, com.amazonaws.services.dynamodbv2.model.Projection)*) extends acceptance.LocalSecondaryIndexes {
  for (index <- indexes) {
    index match {
      case (_index_name, _key_schema, _projection) =>
        add(_index_name, _key_schema, _projection)
      case _ => None
    }
  }
}

class GlobalSecondaryIndexes(indexes:(String, KeySchema, com.amazonaws.services.dynamodbv2.model.Projection, com.amazonaws.services.dynamodbv2.model.ProvisionedThroughput)*) extends acceptance.GlobalSecondaryIndexes {
  for (index <- indexes) {
    index match {
      case (_index_name, _key_schema, _projection, _provisioned_throughput) =>
        add(_index_name, _key_schema, _projection, _provisioned_throughput)
      case _ => None
    }
  }
}

object Condition {
  implicit def alias(condition:Condition.type) = acceptance.Condition
}

object KeyType {
  implicit def alias(key_type:KeyType.type) = acceptance.KeyType
}

object Projection {
  implicit def alias(projection:Projection.type) = acceptance.Projection
}

object ProjectionType {
  implicit def alias(projection_type:ProjectionType.type) = acceptance.ProjectionType
}

object ProvisionedThroughput {
  implicit def alias(provisioned_throughput:ProvisionedThroughput.type) = acceptance.ProvisionedThroughput
}

object Table {
  implicit def alias(table:Table.type) = acceptance.Table
}
