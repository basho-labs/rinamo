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

class HashKeyTest extends FunSpec
  with AWSHelper with MockitoSugar with Matchers
  with BeforeAndAfterEach with AwaitilitySupport {

  val hash_table_name = "test_table_hash"
  var table:CreateTableResult = null
  var item:Item = null

  override def beforeEach() {
    table = Table.create(hash_table_name, "Id", "N")
    await atMost(2, MINUTES) until {
      val result = Table.describe(hash_table_name)
      // TODO:  and these together, once rinamo goes async
      "ACTIVE".equals(result.getTable().getTableStatus())
      hash_table_name.equals(result.getTable().getTableName())
    }
    item = new Item(
        ("Id", "N", "101"),
        ("Title", "S", "Some Title"),
        ("ISBN", "S", "ABC"))
    val item2 = new Item(
        ("Id", "N", "102"),
        ("Title", "S", "Another Title"),
        ("ISBN", "S", "DEF"))
    val item3 = new Item(
        ("Id", "N", "101"),
        ("Title", "S", "Tale of Two Databases"),
        ("ISBN", "S", "XYZ"))

    Table.put(hash_table_name)(item, item2, item3)
  }
  override def afterEach() {
    try {
      Table.delete(hash_table_name)
    }
    catch {
      case e: ResourceNotFoundException => {}
    }
    await atMost(2, MINUTES) until {
      var exception:Throwable = null
      try {
        Table.describe(hash_table_name)
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
        assert(hash_table_name.equals(table.getTableDescription().getTableName()))
      }

      it ("should fail creating an existing table") {
        evaluating {
          Table.create(hash_table_name, "Id", "N")
        } should produce [ResourceInUseException]
      }
    }
    describe ("US193705: manage tables, describe table") {
      it ("should describe a table") {
        val describe_result = Table.describe(hash_table_name)
        assert(hash_table_name.equals(describe_result.getTable().getTableName()))
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
        assert(table_list find { case (x) => x == hash_table_name } isDefined)
      }
    }
    describe ("US193708: manage tables, delete table") {
      it ("should delete a table") {
        val delete_result = Table.delete(hash_table_name)
        assert(hash_table_name.equals(delete_result.getTableDescription().getTableName()))
      }

      it ("should fail deleting a non existing table") {
        evaluating {
          Table.delete("nonexistant_table")
        } should produce [ResourceNotFoundException]
      }
    }
    describe ("US193713: modify data, put item") {
      it ("should put item in table") {
        val result = Table.put(hash_table_name, item)
        assert("{}".equals(result.toString()))
      }

      it ("should fail put if conditional expectation fails") {
        evaluating {
          val invalid_expect = new ExpectedAttributeValue(
              new AttributeValue("Bar")).withExists(true)
          var expected:Map[String, ExpectedAttributeValue] = Map()
          expected += (("Foo", invalid_expect))
          Table.put(hash_table_name, item, Some(expected), None)
        } should produce [ConditionalCheckFailedException]
      }

      it ("should fail put if using non existing table") {
        evaluating {
          Table.put("nonexistant_table", item)
        } should produce [ResourceNotFoundException]
      }

      // ALL_OLD appears to behave like NONE, but different from these
      it ("should fail put if using invalid return values") {
        evaluating {
          Table.put(hash_table_name, item, None, Some(ReturnValue.ALL_NEW))
        } should produce [AmazonServiceException]
        evaluating {
          Table.put(hash_table_name, item, None, Some(ReturnValue.UPDATED_OLD))
        } should produce [AmazonServiceException]
        evaluating {
          Table.put(hash_table_name, item, None, Some(ReturnValue.UPDATED_NEW))
        } should produce [AmazonServiceException]
      }
    }

    describe ("[US193709, US193711]: table type, hash key, LWW behavior") {
      describe ("read data, get item") {
        it ("should read item from table") {
          val result = Table.get(hash_table_name, "Id", "N", "101")
          val title_value = result.getItem().get("Title")
          assert("Tale of Two Databases".equals(title_value.getS()))
        }
      }
      describe ("read data, query item") {
        it ("should query exactly one result") {
          val query_result = Table.query(hash_table_name, "Id", "101")
          assert(query_result.getCount() == 1)
          assert("Tale of Two Databases".equals(query_result.getItems().get(0).get("Title").getS()))
        }
        it ("should ignore range key conditions like amazon") {
          val query_result = Table.query(hash_table_name, "Id", "101",
              Some("Title"), Some("EQ"), Some("Because Amazon."))
          assert("Tale of Two Databases".equals(query_result.getItems().get(0).get("Title").getS()))
        }
      }
    }

    describe ("[US193709, US193711]: table type, hash + range, LWW behavior") {
      describe ("read data, query item") {
        it ("from table using EQ") (pending)
        it ("from table using LE") (pending)
        it ("from table using LT") (pending)
        it ("from table using GE") (pending)
        it ("from table using GT") (pending)
        it ("from table using BEGINS_WITH") (pending)
        it ("from table using BETWEEN") (pending)
      }
    }

    describe ("US193712: read data, scan item") {
      it ("should scan item in table") (pending)
    }
    describe ("US193715: modify data, delete item") {
      it ("should delete item from table") {
        val delete_result = Table.delete(hash_table_name, "Id", "N", "101")
        await atMost(5, SECONDS) until {
          val get_result = Table.get(hash_table_name, "Id", "N", "101")
          get_result.getItem() == null
        }
      }
    }
  }
}
