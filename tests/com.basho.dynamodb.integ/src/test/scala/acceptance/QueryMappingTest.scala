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

import org.scalatest.BeforeAndAfterEach
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import org.mockito.Matchers._

import java.util.concurrent.TimeUnit._

import com.amazonaws._
import com.amazonaws.services.dynamodbv2.model._
import com.jayway.awaitility.scala._
import com.jayway.awaitility.Awaitility._

class QueryMappingTest extends FunSpec
  with AWSHelper with MockitoSugar with Matchers
  with BeforeAndAfterEach with AwaitilitySupport {

  val item_1 = new Item(
    ("Id", "N", "101"),
    ("Title", "S", "Some Title"),
    ("ISBN", "S", "ABC"))
  val item_2 = new Item(
    ("Id", "N", "102"),
    ("Title", "S", "Another Title"),
    ("ISBN", "S", "DEF"))
  val item_3 = new Item(
    ("Id", "N", "101"),
    ("Title", "S", "Tale of Two Databases"),
    ("ISBN", "S", "XYZ"))

    describe ("[US193709, US193711]: table type, hash + range, LWW behavior") {
      val range_table_name = "test_table_range"
      it ("should create range table") {
        val table_range:CreateTableResult = Table.create(range_table_name, "Id", "N", Some("Title"), Some("S"))
        Table.put(range_table_name)(item_1, item_2, item_3)
        await atMost(2, MINUTES) until {
          val result = Table.describe(range_table_name)
          // TODO:  and these together, once rinamo goes async
          "ACTIVE".equals(result.getTable().getTableStatus())
          range_table_name.equals(result.getTable().getTableName())
        }
      }

      describe ("read data, query item") {
        it ("from table using EQ") {
          val query_result = Table.range_query(
              range_table_name, "Id", "101",
              Some("Title"), Some("EQ"), Some("Some Title"))
          assert(query_result.getCount() == 1)
          assert("Some Title".equals(query_result.getItems().get(0).get("Title").getS()))
        }
        it ("from table using LE") {
          val query_result = Table.range_query(
              range_table_name, "Id", "101",
              Some("Title"), Some("LE"), Some("Tale of Two Databases"))
          assert(query_result.getCount() == 2)
        }
        it ("from table using LT") {
          val query_result = Table.range_query(
              range_table_name, "Id", "101",
              Some("Title"), Some("LT"), Some("Tale of Two Databases"))
          assert(query_result.getCount() == 1)
        }
        it ("from table using GE") {
          val query_result = Table.range_query(
              range_table_name, "Id", "101",
              Some("Title"), Some("GE"), Some("Tale of Two Databases"))
          assert(query_result.getCount() == 1)
        }
        it ("from table using GT") {
          val query_result = Table.range_query(
              range_table_name, "Id", "101",
              Some("Title"), Some("GT"), Some("Tale of Two Databases"))
          assert(query_result.getCount() == 0)
        }
        it ("from table using BEGINS_WITH") {
          val query_result = Table.range_query(
              range_table_name, "Id", "101",
              Some("Title"), Some("BEGINS_WITH"), Some("Some"))
          assert(query_result.getCount() == 1)
          assert("Some Title".equals(query_result.getItems().get(0).get("Title").getS()))
        }
        it ("from table using BETWEEN") {
          val query_result = Table.range_query(
              range_table_name, "Id", "101",
              Some("Title"), Some("BETWEEN"), Some("A"), Some("Z"))
          assert(query_result.getCount() == 2)
        }
        it ("finds all range values when using hash key only") {
          val query_result = Table.range_query(range_table_name, "Id", "101")
          assert(query_result.getCount() == 2)
        }
      }

      it ("should delete range table") {
        try {
          Table.delete(range_table_name)
        }
        catch {
          case e: ResourceNotFoundException => {}
        }
        await atMost(2, MINUTES) until {
          var exception:Throwable = null
          try {
            Table.describe(range_table_name)
          }
          catch {
            case e: Throwable => {
              exception = e
            }
          }
          exception != null
        }
      }
  }

}
