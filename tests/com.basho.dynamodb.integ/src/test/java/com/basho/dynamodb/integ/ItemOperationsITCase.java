package com.basho.dynamodb.integ;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import com.amazonaws.ClientConfiguration;
import com.amazonaws.auth.AWSCredentials;
import com.amazonaws.auth.BasicAWSCredentials;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import com.amazonaws.services.dynamodbv2.model.PutItemRequest;
import com.amazonaws.services.dynamodbv2.model.PutItemResult;

public class ItemOperationsITCase {

  @Test
  public void simpleTest() {
    /*
	AWSCredentials credentials = new BasicAWSCredentials("access_key", "secret_key");
	AmazonDynamoDBClient  client = new AmazonDynamoDBClient(credentials);
	client.setEndpoint("http://localhost:8000");
	String tableName = "ProductCatalog";
	Map<String, AttributeValue> item = new HashMap<String, AttributeValue>();
	item.put("Id", new AttributeValue().withN("104"));
	item.put("Title", new AttributeValue().withS("Book 104 Title"));
	item.put("ISBN", new AttributeValue().withS("111-1111111111"));
	item.put("Price", new AttributeValue().withS("25.00"));
	item.put("Authors", new AttributeValue().withSS(Arrays.asList("Author1", "Author2")));
	
	PutItemRequest putItemRequest = new PutItemRequest()
	 .withTableName(tableName)
	 .withItem(item);
	PutItemResult result = client.putItem(putItemRequest);
	*/
	}

}
