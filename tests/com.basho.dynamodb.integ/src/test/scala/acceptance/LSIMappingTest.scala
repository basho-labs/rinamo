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
 * http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LSILowLevelJava.Example.html
 */
class LSIMappingTest extends FunSpec
  with AWSHelper with MockitoSugar with Matchers
  with BeforeAndAfterEach with AwaitilitySupport {

  val table_attributes = new Attributes(
    ("CustomerId", "S"),
    ("OrderId", "N"),
    ("OrderCreationDate", "N"),
    ("IsOpen", "N")
  )

  val tableKey = new KeySchema(
    ("CustomerId", KeyType.HASH),
    ("OrderId", KeyType.RANGE))

  val orderCreationIndexKey = new KeySchema(
    ("CustomerId", KeyType.HASH),
    ("OrderCreationDate", KeyType.RANGE))

  val isOpenIndexKey = new KeySchema(
    ("CustomerId", KeyType.HASH),
    ("IsOpen", KeyType.RANGE))

  val orderCreationIndexProjection = Projection.build_value(ProjectionType.INCLUDE, Some(List("ProductCategory", "ProductName")))
  val isOpenIndexProjection = Projection.build_value(ProjectionType.ALL, None)

  val lsi_secondary_indexes = new LocalSecondaryIndexes (
    ("OrderCreationDateIndex", orderCreationIndexKey, orderCreationIndexProjection),
    ("IsOpenIndex", isOpenIndexKey, isOpenIndexProjection))

  val provisioned_throughput = ProvisionedThroughput.build_value(1L, 1L)

  val item_1 = new Item(
    ("CustomerId", "S", "alice@example.com"),
    ("OrderId", "N", "1"),
    ("IsOpen", "N", "1"),
    ("OrderCreationDate", "N", "20130101"),
    ("ProductCategory", "S", "Book"),
    ("ProductName", "S", "The Great Outdoors"),
    ("OrderStatus", "S", "PACKING ITEMS"))
  
  val item_2 = new Item(
    ("CustomerId", "S", "alice@example.com"),
    ("OrderId", "N", "2"),
    ("IsOpen", "N", "1"),
    ("OrderCreationDate", "N", "20130221"),
    ("ProductCategory", "S", "Bike"),
    ("ProductName", "S", "Super Mountain"),
    ("OrderStatus", "S", "ORDER RECEIVED"))

  val item_3 = new Item(
    ("CustomerId", "S", "alice@example.com"),
    ("OrderId", "N", "3"),
    ("OrderCreationDate", "N", "20130304"),
    ("ProductCategory", "S", "Music"),
    ("ProductName", "S", "A Quiet Interlude"),
    ("OrderStatus", "S", "IN TRANSIT"),
    ("ShipmentTrackingId", "S", "176493"))

  val item_4 = new Item(
    ("CustomerId", "S", "bob@example.com"),
    ("OrderId", "N", "1"),
    ("OrderCreationDate", "N", "20130111"),
    ("ProductCategory", "S", "Movie"),
    ("ProductName", "S", "Calm Before The Storm"),
    ("OrderStatus", "S", "SHIPPING DELAY"),
    ("ShipmentTrackingId", "S", "859323"))

    val item_5 = new Item(
    ("CustomerId", "S", "bob@example.com"),
    ("OrderId", "N", "2"),
    ("OrderCreationDate", "N", "20130124"),
    ("ProductCategory", "S", "Music"),
    ("ProductName", "S", "E-Z Listening"),
    ("OrderStatus", "S", "DELIVERED"),
    ("ShipmentTrackingId", "S", "756943"))

    val item_6 = new Item(
    ("CustomerId", "S", "bob@example.com"),
    ("OrderId", "N", "3"),
    ("OrderCreationDate", "N", "20130221"),
    ("ProductCategory", "S", "Music"),
    ("ProductName", "S", "Symphony 9"),
    ("OrderStatus", "S", "DELIVERED"),
    ("ShipmentTrackingId", "S", "645193"))

    val item_7 = new Item(
    ("CustomerId", "S", "bob@example.com"),
    ("OrderId", "N", "4"),
    ("IsOpen", "N", "1"),
    ("OrderCreationDate", "N", "20130222"),
    ("ProductCategory", "S", "Hardware"),
    ("ProductName", "S", "Extra Heavy Hammer"),
    ("OrderStatus", "S", "PACKING ITEMS"))

    val item_8 = new Item(
    ("CustomerId", "S", "bob@example.com"),
    ("OrderId", "N", "5"),
    ("OrderCreationDate", "N", "20130309"),
    ("ProductCategory", "S", "Book"),
    ("ProductName", "S", "How To Cook"),
    ("OrderStatus", "S", "IN TRANSIT"),
    ("ShipmentTrackingId", "S", "440185"))

    val item_9 = new Item(
    ("CustomerId", "S", "bob@example.com"),
    ("OrderId", "N", "6"),
    ("OrderCreationDate", "N", "20130318"),
    ("ProductCategory", "S", "Luggage"),
    ("ProductName", "S", "Really Big Suitcase"),
    ("OrderStatus", "S", "DELIVERED"),
    ("ShipmentTrackingId", "S", "893927"))

  val item_10 = new Item(
    ("CustomerId", "S", "bob@example.com"),
    ("OrderId", "N", "7"),
    ("OrderCreationDate", "N", "20130324"),
    ("ProductCategory", "S", "Golf"),
    ("ProductName", "S", "PGA Pro II"),
    ("OrderStatus", "S", "OUT FOR DELIVERY"),
    ("ShipmentTrackingId", "S", "383283"))

  describe ("[US247961]: table type, lsi, LWW behavior") {
    val table_name = "test_table_lsi"
    it ("should create lsi table") {
      logger.info("Creating Table ...")
      val table_lsi:CreateTableResult = Table.create(
          table_name, tableKey, table_attributes,
          Some(lsi_secondary_indexes), None, provisioned_throughput)
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
    describe ("lsi operations") {
      it ("should add data and build table indicies") {
        Table.put(table_name)(item_1, item_2, item_3, item_4, item_5,
                                  item_6, item_7, item_8, item_9, item_10)
      }
      
      it ("should query bob's open orders") {
        val key_conditions_1 = new KeyConditions(
          ("CustomerId", "S", "EQ", "bob@example.com", None),
          ("IsOpen", "N", "EQ", "1", None))
        val results_1 = Table.query(table_name, key_conditions_1, Some("IsOpenIndex"))
        assert(1 == results_1.getCount())
      }
      
      it ("should query bob's orders created after a specific date time") {
        val key_conditions_2 = new KeyConditions(
          ("CustomerId", "S", "EQ", "bob@example.com", None),
          ("OrderCreationDate", "N", "GT", "20130131", None))
        val results_2 = Table.query(table_name, key_conditions_2, Some("OrderCreationDateIndex"))
        assert(5 == results_2.getCount())
      }
    }
    it ("should delete lsi table") {
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