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
    try {
      client.createTable(createHashKeyNTable("one", "One-Id", 10L, 5L));
      client.createTable(createHashKeyNTable("two", "Two-Id", 10L, 5L));
      client.createTable(createHashKeyNTable("three", "Three-Id", 10L, 5L));

      // tables created?
      await().atMost(5, SECONDS).until(tableExists("one"));
      await().atMost(5, SECONDS).until(tableExists("two"));
      await().atMost(5, SECONDS).until(tableExists("three"));
      
      // each table in list?
      await().atMost(5, SECONDS).until(tableInList("one"));
      await().atMost(5, SECONDS).until(tableInList("two"));
      await().atMost(5, SECONDS).until(tableInList("three"));

      ListTablesResult result = client.listTables();
      assertEquals(3, result.getTableNames().size());

      result = client.listTables(1);
      assertEquals(1, result.getTableNames().size());

      // TODO: Passes on DynamoDB; not on Rinamo
      //ListTablesResult result = client.listTables("one", 1);
      //assertEquals(1, result.getTableNames().size());
      //assertEquals("three", result.getLastEvaluatedTableName());
    }
    finally {
      client.deleteTable("one");
      await().atMost(5, SECONDS).until(tableDoesNotExist("one"));

      client.deleteTable("two");
      await().atMost(5, SECONDS).until(tableDoesNotExist("two"));

      client.deleteTable("three");
      await().atMost(5, SECONDS).until(tableDoesNotExist("three"));
    }
    
  }
}
