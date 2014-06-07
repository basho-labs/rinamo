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

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import com.amazonaws.services.dynamodbv2.model._

// TODO:  add await for result to converge
Table.create("books_range_bulk", "Id", "N", Some("Title"), Some("S"))

// Index Shape Controls
val t_items = 10
val per_item = 1000

val tasks: Seq[Future[Array[PutItemResult]]] =
  for (i <- 1 to t_items) yield future {
    println("Executing task " + i)
    val results: Array[PutItemResult] = Array()
    for (j <- 1 to per_item) {
      val item = new Item(
        ("Id", "N", i.toString()),
        ("Title", "S", "Title " + i.toString() + "::" + j.toString()),
        ("ISBN", "S", i.toString() + "::" + j.toString()))
      results :+ Table.put("books_range_bulk", item)
    }
    results
  }

val aggregated: Future[Seq[Array[PutItemResult]]] =
  Future.sequence(tasks)

val results: Seq[Array[PutItemResult]] =
  Await.result(aggregated, 300.seconds)

val result:QueryResult = Table.range_query("books_range_bulk", "Id", "1", Some("Title"), Some("BEGINS_WITH"), Some("Title 1"))

// TODO:  add await for result count to converge to #per_item count
// assert(per_item == result.getCount())
