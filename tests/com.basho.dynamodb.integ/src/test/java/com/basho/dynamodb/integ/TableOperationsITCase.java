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

package com.basho.dynamodb.integ;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.amazonaws.services.dynamodbv2.model.CreateTableResult;
import com.amazonaws.services.dynamodbv2.model.ListTablesResult;
import com.amazonaws.services.dynamodbv2.model.ResourceNotFoundException;

import static com.jayway.awaitility.Awaitility.await;
import static java.util.concurrent.TimeUnit.SECONDS;

public class TableOperationsITCase extends DynamoDBTest {
  @Before
  public void before() {
    super.before();
    tableName = "TableOpsTestTable";
  }

  @After
  public void after() {
    super.after();
    try {
      client.deleteTable(tableName);
      await().atMost(5, SECONDS).until(tableDoesNotExist(tableName));
    }
    catch (ResourceNotFoundException e) { /* nothing to delete */ }
  }

  @Test
  public void createTableUsingHashKeyTest() {
    String hashKeyName = "Id";

    CreateTableResult result = client.createTable(
      createHashKeyNTable(tableName, hashKeyName, 10L, 5L));
    assertNotNull(result);

    await().atMost(5, SECONDS).until(tableExists(tableName));
    await().atMost(5, SECONDS).until(tableInList(tableName));
  }

  @Test
  public void listMultipleTablesTest() {
    int count = 5;
    try {
      createAndTest(count);

      ListTablesResult result = client.listTables();
      assertEquals(count, result.getTableNames().size());

      result = client.listTables(3);
      assertEquals(3, result.getTableNames().size());
      assertEquals("key_2", result.getLastEvaluatedTableName());

      result = client.listTables("key_1", 1);
      assertEquals(1, result.getTableNames().size());
      assertEquals("key_2", result.getTableNames().get(0));
      assertEquals("key_2", result.getLastEvaluatedTableName());
    }
    finally {
      delete(count);
    }
  }

  private void createAndTest(int times) {
    for (int i = 0; i < times; i++) {
      client.createTable(
        createHashKeyNTable("key_" + Integer.toString(i), i + "-Id", 10L, 5L));
    }

    for (int i = 0; i < times; i++) {
      await().atMost(5, SECONDS).until(tableExists("key_" + Integer.toString(i)));
      await().atMost(5, SECONDS).until(tableInList("key_" + Integer.toString(i)));
    }
  }

  private void delete(int times) {
    for (int i = 0; i < times; i++) {
      client.deleteTable("key_" + Integer.toString(i));
      await().atMost(5, SECONDS).until(tableDoesNotExist("key_" + Integer.toString(i)));
    }
  }

}
