package com.basho.dynamodb.integ;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.PropertiesCredentials;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.model.AttributeDefinition;
import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import com.amazonaws.services.dynamodbv2.model.CreateTableRequest;
import com.amazonaws.services.dynamodbv2.model.CreateTableResult;
import com.amazonaws.services.dynamodbv2.model.DeleteTableResult;
import com.amazonaws.services.dynamodbv2.model.DescribeTableResult;
import com.amazonaws.services.dynamodbv2.model.KeySchemaElement;
import com.amazonaws.services.dynamodbv2.model.KeyType;
import com.amazonaws.services.dynamodbv2.model.ListTablesResult;
import com.amazonaws.services.dynamodbv2.model.ProvisionedThroughput;
import com.amazonaws.services.dynamodbv2.model.PutItemRequest;
import com.amazonaws.services.dynamodbv2.model.PutItemResult;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class TableOperationsITCase {

  private AmazonDynamoDBClient client;
  
  private String tableName;

  @Before
  public void before() {
    AWSCredentials creds = null;
    try {
    creds = new PropertiesCredentials(
                TableOperationsITCase.class.getResourceAsStream("AwsCredentials.properties"));
    }
    catch(IOException e) {
      e.printStackTrace();
    }

    client = new AmazonDynamoDBClient(creds);
    client.setEndpoint("http://localhost:8098/rinamo");
//    client.setEndpoint("http://localhost:8000");
    
    tableName = "TestTable";
  }

  @After
  public void after() {
    
  }
  
  @Test
  public void startupTest() {
    assertTrue(true);
  }

  @Test
  public void createTableTest() {
    String hashKeyName = "Id", hashKeyType = "N";
    String rangeKeyName = null, rangeKeyType = null;
    long readCapacityUnits = 10L, writeCapacityUnits = 5L;

    ArrayList<KeySchemaElement> ks = new ArrayList<KeySchemaElement>();
    ArrayList<AttributeDefinition> attributeDefinitions = new ArrayList<AttributeDefinition>();

    ks.add(new KeySchemaElement().withAttributeName(hashKeyName).withKeyType(KeyType.HASH));
    attributeDefinitions.add(new AttributeDefinition().withAttributeName(hashKeyName).withAttributeType(hashKeyType));

    if (rangeKeyName != null){
      ks.add(new KeySchemaElement().withAttributeName(rangeKeyName).withKeyType(KeyType.RANGE));
      attributeDefinitions.add(new AttributeDefinition().withAttributeName(rangeKeyName).withAttributeType(rangeKeyType));
    }

    // Provide initial provisioned throughput values as Java long data types
    ProvisionedThroughput provisionedthroughput = new ProvisionedThroughput()
      .withReadCapacityUnits(readCapacityUnits)
      .withWriteCapacityUnits(writeCapacityUnits);

    CreateTableRequest request = new CreateTableRequest()
      .withTableName(tableName)
      .withKeySchema(ks)
      .withProvisionedThroughput(provisionedthroughput);

    request.setAttributeDefinitions(attributeDefinitions);

    CreateTableResult result = client.createTable(request);
    assertNotNull(result);
  }

  @Test
  public void listTablesTest() {
    ListTablesResult tables = client.listTables();
    assertNotNull(tables);
  }

  @Test
  public void describeTableTest() {
    DescribeTableResult result = client.describeTable(tableName);
    assertNotNull(result);
  }
  
  @Test
  public void putItemTest() {
    Map<String, AttributeValue> item = new HashMap<String, AttributeValue>();
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

  @Test
  public void deleteTableTest() {
    DeleteTableResult result = client.deleteTable(tableName);
    assertNotNull(result);
  }

}
