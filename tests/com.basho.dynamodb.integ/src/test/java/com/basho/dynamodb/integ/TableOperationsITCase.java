package com.basho.dynamodb.integ;

import java.io.IOException;
import java.util.ArrayList;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.PropertiesCredentials;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.model.AttributeDefinition;
import com.amazonaws.services.dynamodbv2.model.CreateTableRequest;
import com.amazonaws.services.dynamodbv2.model.CreateTableResult;
import com.amazonaws.services.dynamodbv2.model.DescribeTableResult;
import com.amazonaws.services.dynamodbv2.model.KeySchemaElement;
import com.amazonaws.services.dynamodbv2.model.KeyType;
import com.amazonaws.services.dynamodbv2.model.ListTablesResult;
import com.amazonaws.services.dynamodbv2.model.ProvisionedThroughput;

import static org.junit.Assert.assertNotNull;

public class TableOperationsITCase {

  private AmazonDynamoDBClient client;

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
  }

  @After
  public void after() {
  }

  @Test
  public void createTableTest() {
    String tableName = "TestTable";
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
    DescribeTableResult result = client.describeTable("ProductCatalog");
    assertNotNull(result);
  }
}
