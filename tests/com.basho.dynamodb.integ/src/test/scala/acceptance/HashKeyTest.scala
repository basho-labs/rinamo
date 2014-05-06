package acceptance

import org.scalatest.BeforeAndAfterEach
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import org.mockito.Matchers._

import java.util.concurrent.TimeUnit._

import com.amazonaws.services.dynamodbv2.model._
import com.jayway.awaitility.scala._
import com.jayway.awaitility.Awaitility._

class HashKeyTest extends FunSpec
  with AWSHelper with MockitoSugar with Matchers
  with BeforeAndAfterEach with AwaitilitySupport {

  val table_name = "test_table_hash"
  var table:CreateTableResult = null

  override def beforeEach() {
    table = Table.create(table_name, "Id", "N")
    await atMost(500, MILLISECONDS) until {
      val result = Table.describe(table_name)
      table_name.equals(result.getTable().getTableName())
    }
  }
  override def afterEach() {
    try {
      Table.delete(table_name)
    }
    catch {
      case e: ResourceNotFoundException => {}
    }
    await atMost(500, MILLISECONDS) until {
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

  describe ("test uses dynamodb api, table using hash key") {
    describe ("US193704: manage tables, create table") {
      it ("should create table") {
        assert(table_name.equals(table.getTableDescription().getTableName()))
      }

      it ("should fail creating an existing table") {
        evaluating {
          Table.create(table_name, "Id", "N")
        } should produce [ResourceInUseException]
      }
    }
    describe ("US193705: manage tables, describe table") {
      it ("should describe a table") {
        val describe_result = Table.describe(table_name)
        assert(table_name.equals(describe_result.getTable().getTableName()))
      }

      it ("should fail describing a non existing table") {
        evaluating {
          Table.describe("nonexistant_table")
        } should produce [ResourceNotFoundException]
      }
    }
    describe ("US193707: manage tables, list tables") {
      it ("should list tables") {
        val table_list = Table.list()
        assert(table_list find { case (x) => x == table_name } isDefined)
      }
    }
    describe ("US193708: manage tables, delete table") {
      it ("should delete a table") {
        val delete_result = Table.delete(table_name)
        assert(table_name.equals(delete_result.getTableDescription().getTableName()))
      }

      it ("should fail deleting a non existing table") {
        evaluating {
          Table.delete("nonexistant_table")
        } should produce [ResourceNotFoundException]
      }
    }
    describe ("US193713: modify data, put item") {
      val item = new Item()
      item.add("Id", "N", "101")
      item.add("Title", "S", "Some Title")

      it ("should put item in table") (pending)

      it ("should fail put if using non existing table") {
        evaluating {
          Table.put("nonexistant_table", item)
        } should produce [ResourceNotFoundException]
      }
    }
    describe ("US193709: read data, get item") {
      it ("should read item from table") (pending)
    }
    describe ("US193711: read data, query item") {
      it ("should query items from table") (pending)
    }
    describe ("US193712: read data, scan item") {
      it ("should scan item in table") (pending)
    }
    describe ("US193715: modify data, delete item") {
      it ("should delete item from table") (pending)
    }
  }
}
