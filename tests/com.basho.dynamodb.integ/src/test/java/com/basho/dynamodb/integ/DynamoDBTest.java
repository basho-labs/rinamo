package com.basho.dynamodb.integ;

import static org.junit.Assert.assertNotNull;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

import org.junit.After;
import org.junit.Before;

import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.PropertiesCredentials;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.model.AttributeDefinition;
import com.amazonaws.services.dynamodbv2.model.CreateTableRequest;
import com.amazonaws.services.dynamodbv2.model.DescribeTableResult;
import com.amazonaws.services.dynamodbv2.model.KeySchemaElement;
import com.amazonaws.services.dynamodbv2.model.KeyType;
import com.amazonaws.services.dynamodbv2.model.ListTablesResult;
import com.amazonaws.services.dynamodbv2.model.ProvisionedThroughput;
import com.amazonaws.services.dynamodbv2.model.ResourceNotFoundException;

public class DynamoDBTest {
  protected AmazonDynamoDBClient client;
  protected String tableName;

  @Before
  public void before() {
    AWSCredentials creds = null;
    try {
    creds = new PropertiesCredentials(
                DynamoDBTest.class.getResourceAsStream("AwsCredentials.properties"));
    }
    catch(IOException e) {
      e.printStackTrace();
    }

    client = new AmazonDynamoDBClient(creds);
    client.setEndpoint("http://localhost:8000");
  }
  
  @After
  public void after() {
  }
  
  protected CreateTableRequest createHashKeyNTable(String newTable, String hashKeyName, long rc, long wc) {
    ArrayList<KeySchemaElement> ks = new ArrayList<KeySchemaElement>();
    ArrayList<AttributeDefinition> attributeDefinitions = new ArrayList<AttributeDefinition>();

    ks.add(new KeySchemaElement().withAttributeName(hashKeyName).withKeyType(KeyType.HASH));
    attributeDefinitions.add(new AttributeDefinition().withAttributeName(hashKeyName).withAttributeType("N"));

    ProvisionedThroughput provisionedthroughput = new ProvisionedThroughput()
      .withReadCapacityUnits(rc)
      .withWriteCapacityUnits(wc);

    CreateTableRequest request = new CreateTableRequest()
      .withTableName(newTable)
      .withKeySchema(ks)
      .withProvisionedThroughput(provisionedthroughput);

    request.setAttributeDefinitions(attributeDefinitions);
    return request;
  }

  protected Callable<Boolean> tableExists(final String table) {
    return new Callable<Boolean>() {
      public Boolean call() {
        DescribeTableResult result = client.describeTable(table);
        assertNotNull(result);
        return result.getTable().getTableName().equals(table);
      }
    };
  }
  
  protected Callable<Boolean> tableInList(final String table) {
    return new Callable<Boolean>() {
      public Boolean call() {
        ListTablesResult tables = client.listTables();
        assertNotNull(tables);
        List<String> tableList = tables.getTableNames();
        return tableList.contains(table);
      }
    };
  }

  protected Callable<Boolean> tableDoesNotExist(final String table) {
    return new Callable<Boolean>() {
      public Boolean call() {
        boolean threwException = false;
        try {
          client.describeTable(table);
        }
        catch (ResourceNotFoundException e) {
          threwException = true;
        }
        return threwException;
      }
    };
  }

}
