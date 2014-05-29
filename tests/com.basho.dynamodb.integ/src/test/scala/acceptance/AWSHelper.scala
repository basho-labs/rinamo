package acceptance

import com.amazonaws._
import com.amazonaws.auth._
import com.amazonaws.services.dynamodbv2._
import com.amazonaws.services.dynamodbv2.model._

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import bootstrap.rinamo._

trait AWSHelper {
  val client: AmazonDynamoDBClient = { RinamoConsole.config }

  // enable test side logging using profile: -Ptest-debug-logging
  val logger: Logger = LoggerFactory.getLogger(this.getClass.getName)
}
object AWSHelper extends AWSHelper