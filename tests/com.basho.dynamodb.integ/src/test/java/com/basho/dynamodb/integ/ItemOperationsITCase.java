package com.basho.dynamodb.integ;

import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import com.amazonaws.services.dynamodbv2.model.CreateTableResult;
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
    CreateTableResult result = client.createTable(
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
//  item.put("Id", new AttributeValue().withSS(Arrays.asList("1", "2", "3")));
    item.put("Id", new AttributeValue().withN("101"));
    item.put("Title", new AttributeValue().withS("Book 101 Title"));
    item.put("ISBN", new AttributeValue().withS("111-1111111111"));
    item.put("Authors", new AttributeValue().withSS(Arrays.asList("Author1")));
    item.put("Price", new AttributeValue().withN("2"));
    item.put("Dimensions", new AttributeValue().withS("8.5 x 11.0 x 0.5"));
    item.put("PageCount", new AttributeValue().withN("500"));
    item.put("InPublication", new AttributeValue().withN("1"));
    item.put("ProductCategory", new AttributeValue().withS("Book"));
    PutItemRequest pir = new PutItemRequest().withItem(item);
    pir.setTableName(tableName);
    PutItemResult result = client.putItem(pir);
    assertNotNull(result);
  }
  
  @Test
  public void putItemNoTableTest() {
    Map<String, AttributeValue> item = new HashMap<String, AttributeValue>();

    PutItemRequest pir = new PutItemRequest().withItem(item);    
    PutItemResult result = client.putItem(pir);
    assertNotNull(result);
  }

}
