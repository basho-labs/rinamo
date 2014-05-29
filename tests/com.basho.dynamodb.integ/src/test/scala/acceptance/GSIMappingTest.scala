package acceptance

import org.scalatest.BeforeAndAfterEach
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import org.mockito.Matchers._

import java.util.concurrent.TimeUnit._

import com.amazonaws._
import com.amazonaws.services.dynamodbv2.model._
import com.jayway.awaitility.scala._
import com.jayway.awaitility.Awaitility._
/*
 * Test modeled after:
 * http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GSILowLevelJava.Example.html
 */
class GSIMappingTest extends FunSpec
  with AWSHelper with MockitoSugar with Matchers
  with BeforeAndAfterEach with AwaitilitySupport {
  
  val table_attributes = new Attributes(
    ("IssueId", "S"),
    ("Title", "S"),
    ("CreateDate", "S"),
    ("DueDate", "S")
  )

  val table_key = new KeySchema(
    ("IssueId", KeyType.HASH),
    ("Title", KeyType.RANGE))

  val createDateIndexKey = new KeySchema(
    ("CreateDate", KeyType.HASH),
    ("IssueId", KeyType.RANGE))
  
  val titleIndexKey = new KeySchema(
    ("Title", KeyType.HASH),
    ("IssueId", KeyType.RANGE))

  val dueDateIndexKey = new KeySchema(
    ("DueDate", KeyType.HASH))

  val createDateIndexProjection = Projection.build_value(ProjectionType.INCLUDE, Some(List("Description", "Status")))
  val titleIndexProjection = Projection.build_value(ProjectionType.KEYS_ONLY , None)
  val dueDateIndexProjection = Projection.build_value(ProjectionType.ALL, None)
  
  val provisioned_throughput = ProvisionedThroughput.build_value(1L, 1L)

  val gsi_secondary_indexes = new GlobalSecondaryIndexes (
    ("CreateDateIndex", createDateIndexKey, createDateIndexProjection, provisioned_throughput),
    ("TitleIndex", titleIndexKey, titleIndexProjection, provisioned_throughput),
    ("DueDateIndex", dueDateIndexKey, dueDateIndexProjection, provisioned_throughput))
 
  val table_provisioned_throughput = ProvisionedThroughput.build_value(1L, 1L)
  
  val item_1 = new Item(
    ("IssueId", "S", "A-101"),
    ("Title", "S", "Compilation error"),
    ("Description", "S", "Can't compile Project X - bad version number. What does this mean?"),
    ("CreateDate", "S", "2013-11-01"),
    ("LastUpdateDate", "S", "2013-11-02"),
    ("DueDate", "S", "2013-11-10"),
    ("Priority", "N", "1"),
    ("Status", "S", "Assigned"))

  val item_2 = new Item(
    ("IssueId", "S", "A-102"),
    ("Title", "S", "Can't read data file"),
    ("Description", "S", "The main data file is missing, or the permissions are incorrect"),
    ("CreateDate", "S", "2013-11-01"),
    ("LastUpdateDate", "S", "2013-11-04"),
    ("DueDate", "S", "2013-11-30"),
    ("Priority", "N", "2"),
    ("Status", "S", "In progress"))

  val item_3 = new Item(
    ("IssueId", "S", "A-103"),
    ("Title", "S", "Test failure"),
    ("Description", "S", "Functional test of Project X produces errors"),
    ("CreateDate", "S", "2013-11-01"),
    ("LastUpdateDate", "S", "2013-11-02"),
    ("DueDate", "S", "2013-11-10"),
    ("Priority", "N", "1"),
    ("Status", "S", "In progress"))

  val item_4 = new Item(
    ("IssueId", "S", "A-104"),
    ("Title", "S", "Compilation error"),
    ("Description", "S", "Variable 'messageCount' was not initialized."),
    ("CreateDate", "S", "2013-11-15"),
    ("LastUpdateDate", "S", "2013-11-16"),
    ("DueDate", "S", "2013-11-30"),
    ("Priority", "N", "3"),
    ("Status", "S", "Assigned"))

  val item_5 = new Item(
    ("IssueId", "S", "A-105"),
    ("Title", "S", "Network Issue"),
    ("Description", "S", "Can't ping IP address 127.0.0.1. Please fix this."),
    ("CreateDate", "S", "2013-11-15"),
    ("LastUpdateDate", "S", "2013-11-16"),
    ("DueDate", "S", "2013-11-19"),
    ("Priority", "N", "5"),
    ("Status", "S", "Assigned"))

  describe ("[???]: table type, gsi, LWW behavior") {
    val table_name = "test_table_gsi"
    it ("should create gsi table") {
      logger.info("Creating Table ...")
      val table_gsi:CreateTableResult = Table.create(
          table_name, table_key, table_attributes,
          None, Some(gsi_secondary_indexes), provisioned_throughput)
      await atMost(2, MINUTES) until { tableLoaded }
    }
    def tableLoaded(): Boolean = {
      try {
        val result = Table.describe(table_name)
        table_name.equals(result.getTable().getTableName())
      }
      catch {
        case e: Throwable => false;
      }
    }
    describe ("gsi operations") {
      it ("should add data and build table indicies") {
        Table.put(table_name)(item_1, item_2, item_3, item_4, item_5)
      }
      
      it ("should query by create date index using two conditions") {  
        val key_conditions_1 = new KeyConditions(
          ("CreateDate", "S", "EQ", "2013-11-01", None),
          ("IssueId", "S", "BEGINS_WITH", "A-", None))
        val results_1 = Table.query(table_name, key_conditions_1, Some("CreateDateIndex"))
        assert(3 == results_1.getCount())
      }
      
      it ("should query by title index using two conditions") {
        val key_conditions_2 = new KeyConditions(
          ("Title", "S", "EQ", "Compilation error", None),
          ("IssueId", "S", "BEGINS_WITH", "A-", None))
        val results_2 = Table.query(table_name, key_conditions_2, Some("TitleIndex"))
        assert(2 == results_2.getCount())
      }
      
      it ("should query by due date index using one condition") {
        val key_conditions_3 = new KeyConditions(
          ("DueDate", "S", "EQ", "2013-11-30", None))
        val results_3 = Table.query(table_name, key_conditions_3, Some("DueDateIndex"))
        assert(2 == results_3.getCount())
      }
    }
    it ("should delete gsi table") {
      try {
        Table.delete(table_name)
      }
      catch {
        case e: ResourceNotFoundException => {}
      }
      await atMost(2, MINUTES) until {
        var exception:Throwable = null
        try {
          Table.describe(table_name)
        }
        catch {
          case e: Throwable => {
            exception = e
          }
        }
        exception != null
      }
    }
  }
}