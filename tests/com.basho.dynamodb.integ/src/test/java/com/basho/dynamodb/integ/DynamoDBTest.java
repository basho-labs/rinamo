package com.basho.dynamodb.integ;

import static org.junit.Assert.assertNotNull;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.Callable;

import org.junit.After;
import org.junit.Before;

import com.amazonaws.ClientConfiguration;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.PropertiesCredentials;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.model.AttributeDefinition;
import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import com.amazonaws.services.dynamodbv2.model.CreateTableRequest;
import com.amazonaws.services.dynamodbv2.model.DescribeTableResult;
import com.amazonaws.services.dynamodbv2.model.GetItemRequest;
import com.amazonaws.services.dynamodbv2.model.GetItemResult;
import com.amazonaws.services.dynamodbv2.model.KeySchemaElement;
import com.amazonaws.services.dynamodbv2.model.KeyType;
import com.amazonaws.services.dynamodbv2.model.ListTablesResult;
import com.amazonaws.services.dynamodbv2.model.ProvisionedThroughput;
import com.amazonaws.services.dynamodbv2.model.ResourceNotFoundException;

public class DynamoDBTest {
  protected AmazonDynamoDBClient client;
  protected String tableName;
  
  public static String getConfigValue(String key, Properties props) {
    String rtnval = null;
    if (props.containsKey(key)) {
      rtnval = props.getProperty(key);
    }
    String possible_env_prop = key.toUpperCase();
    if (System.getenv(possible_env_prop) != null) {
      rtnval = System.getenv(key.toUpperCase());
    }
    return rtnval;
  }

  @Before
  public void before() {
    AWSCredentials creds = null;
    Properties props = null;
    try {
      InputStream in = DynamoDBTest.class.getResourceAsStream("AwsCredentials.properties");
      in.mark(Integer.MAX_VALUE);
      props = new Properties();
      props.load(in);
      in.reset();
      creds = new PropertiesCredentials(in);
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    
    String protocol = getConfigValue("protocol", props);
    String host = getConfigValue("host", props);
    String port = getConfigValue("port", props);
    String root_path = getConfigValue("root_path", props);

    ClientConfiguration config = new ClientConfiguration();
    client = new AmazonDynamoDBClient(creds, config);
    if (host != null && host.length() > 0) {
      client.setEndpoint(protocol + "://" + host + ":" + port + root_path);
    }
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

  protected Callable<Boolean> itemExists(final String key_attr, int key_value,
                                         final String v_attr, final String v_value) {
    final String i_str = Integer.toString(key_value);
    return new Callable<Boolean>() {
      public Boolean call() {
        GetItemRequest gir = new GetItemRequest()
          .addKeyEntry(key_attr, new AttributeValue().withN(i_str));
        gir.setTableName(tableName);
        GetItemResult get_result = client.getItem(gir);
        String result = null;
        try {
          result = get_result.getItem().get(v_attr).getS();
          if (result != null)
            return result.equals(v_value);
        }
        catch (Throwable t) { ; }
        return false;
      }
    };
  }
  protected Callable<Boolean> itemDoesNotExist(final String key_attr, int key_value) {
    final String i_str = Integer.toString(key_value);
    return new Callable<Boolean>() {
      public Boolean call() {
        GetItemRequest gir = new GetItemRequest()
          .addKeyEntry(key_attr, new AttributeValue().withN(i_str));
        gir.setTableName(tableName);
        GetItemResult get_result = client.getItem(gir);
        return get_result.getItem() == null;
      }
    };
  }


}
