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

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.amazonaws.AmazonServiceException;
import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import com.amazonaws.services.dynamodbv2.model.DeleteItemRequest;
import com.amazonaws.services.dynamodbv2.model.GetItemRequest;
import com.amazonaws.services.dynamodbv2.model.GetItemResult;
import com.amazonaws.services.dynamodbv2.model.PutItemRequest;
import com.amazonaws.services.dynamodbv2.model.PutItemResult;
import com.amazonaws.services.dynamodbv2.model.ResourceNotFoundException;

import static com.jayway.awaitility.Awaitility.await;
import static java.util.concurrent.TimeUnit.SECONDS;

public class ItemOperationsITCase extends DynamoDBTest {
  @Before
  public void before() {
    super.before();
    tableName = "ItemOpsTestTable";

    String hashKeyName = "Id";
    client.createTable(
      createHashKeyNTable(tableName, hashKeyName, 10L, 5L));
    await().atMost(5, SECONDS).until(tableExists(tableName));
    await().atMost(5, SECONDS).until(tableInList(tableName));
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
  public void putItemTest() {
    Map<String, AttributeValue> item = new HashMap<String, AttributeValue>();
    // Key datatypes must be scalars, not sets
    item.put("Id", new AttributeValue().withN("101"));
    item.put("Title", new AttributeValue().withS("Book 101 Title"));
    item.put("ISBN", new AttributeValue().withS("111-1111111111"));
    item.put("Authors", new AttributeValue().withSS(Arrays.asList("Author1")));
    item.put("Price", new AttributeValue().withN("2"));
    item.put("Dimensions", new AttributeValue().withS("8.5 x 11.0 x 0.5"));
    item.put("PageCount", new AttributeValue().withN("500"));
    item.put("InPublication", new AttributeValue().withN("1"));
    item.put("ProductCategory", new AttributeValue().withS("Book"));

    try {
      PutItemRequest pir = new PutItemRequest().withItem(item);
      pir.setTableName(tableName);
      PutItemResult result = client.putItem(pir);
      assertNotNull(result);
      await().atMost(5, SECONDS).until(itemExists("Id", 101, "Title", "Book 101 Title"));
    }
    finally {
      DeleteItemRequest dir = new DeleteItemRequest().addKeyEntry("Id", new AttributeValue().withN("101"));
      dir.setTableName(tableName);
      client.deleteItem(dir);
      await().atMost(5, SECONDS).until(itemDoesNotExist("Id", 101));
    }
  }

  @Test
  public void putItemExplodesIfNoTableTest() {
    Map<String, AttributeValue> item = new HashMap<String, AttributeValue>();
    PutItemRequest pir = new PutItemRequest().withItem(item);
    Throwable expected = null;
    try {
      PutItemResult result = client.putItem(pir);
      assertNotNull(result);
    }
    catch(AmazonServiceException e) { expected = e; }
    assertNotNull(expected);
  }

  @Test
  public void getItemTest() {
    Map<String, AttributeValue> attrs = new HashMap<String, AttributeValue>();
    attrs.put("Id", new AttributeValue().withN("101"));
    GetItemRequest gir = new GetItemRequest().addKeyEntry("Id", new AttributeValue().withN("101"));
    gir.setTableName(tableName);
    gir.setConsistentRead(false);
    GetItemResult result = client.getItem(gir);
    assertNotNull(result);
    Map<String,AttributeValue> item = result.getItem();
    assertNull(item);
  }

}
