package acceptance

import scala.beans.BeanProperty
import org.scalatest.BeforeAndAfterEach
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import org.mockito.Matchers._

import com.amazonaws.services.dynamodbv2.model._

class HashKeyTest extends FunSpec with AWSHelper with MockitoSugar with Matchers with BeforeAndAfterEach {
  val table_name = "books_hash"
  var table:CreateTableResult = null

  // todo:  better test idempotence
  // (using poor man's sleep for now)
  override def beforeEach() {
    table = Table.create(table_name, "Id", "N")
    Thread.sleep(5)
  }
  override def afterEach() {
    try {
      Table.delete("books_hash")
      Thread.sleep(5)
    }
    finally {}
   }

  describe ("test uses dynamodb api, table using hash key") {
    val table_name = "books_hash"
    describe ("US193704: manage tables, create table") {
      it ("should create table") {
        assert(table_name.equals(table.getTableDescription().getTableName()))
      }

      it ("should fail creating an existing table") (pending)
    }
    describe ("US193705: manage tables, describe table") {
      it ("should describe a table") {
        val describe_result = Table.describe(table_name)
        assert(table_name.equals(describe_result.getTable().getTableName()))
      }

      it ("should fail describing a non existing table") (pending)
    }
    describe ("US193707: manage tables, list tables") {
      it ("should list tables") {
        val table_list = Table.list()
        assert(table_list find { case (x) => x == "books_hash" } isDefined)
      }
    }
    describe ("US193708: manage tables, delete table") {
      it ("should delete a table") (pending)

      it ("should fail deleting a non existing table") (pending)
    }
    describe ("US193713: modify data, put item") {
      it ("should put item in table") (pending)
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
