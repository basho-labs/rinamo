package acceptance

import scala.beans.BeanProperty
import org.scalatest.BeforeAndAfter
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.scalatest.BeforeAndAfterEach

class HashKeyTest extends FunSpec with AWSHelper with MockitoSugar with Matchers with BeforeAndAfterEach {
  override def beforeEach() {
    Table.delete("books_hash")
   }
  
  describe ("test uses dynamodb api, table using hash key") {
    val table = Table.create("books_hash", "Id", "N")
    describe ("US193704: manage tables") {
      it ("should create table") {
        assert("books_hash".equals(table.getTableDescription().getTableName()))
      }
    }
    describe ("US193705: manage tables, describe table") {
    }
    describe ("US193707: manage tables, list tables") {
    }
    describe ("US193708: manage tables, delete table") {
    }
    describe ("US193713: modify data, put item") {
    }
    describe ("US193709: read data, get item") {
    }
    describe ("US193711: read data, query item") {
    }
    describe ("US193712: read data, scan item") {
    }
    describe ("US193715: modify data, delete item") {
    }
  }
}
