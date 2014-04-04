import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

import com.amazonaws.auth._
import com.amazonaws.services.dynamodbv2._
import com.amazonaws.services.dynamodbv2.model._

class Item {
  private var map: Map[String, AttributeValue] = Map()

  def add(_name:String, _type:String, _value:String): Item = {
    val value = Item.build_value(_name, _type, _value)
    map += value
    return this
  }
  
  override def toString: String = {
    map.toString
  }
  
  def asMap():Map[String, AttributeValue] = {
    return map
  }
}

object Item {
  def build_value(_name:String, _type:String, _value:String):(String,AttributeValue) = {
    return _type match {
      case "N" => (_name, new AttributeValue().withN(_value))
      case "S" => (_name, new AttributeValue().withS(_value))
    }
  }  
}