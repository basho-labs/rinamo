package acceptance

import com.amazonaws._
import com.amazonaws.auth._
import com.amazonaws.services.dynamodbv2._
import com.amazonaws.services.dynamodbv2.model._

import bootstrap.rinamo._

trait AWSHelper {
  val client: AmazonDynamoDBClient = { RinamoConsole.config }
}
object AWSHelper extends AWSHelper