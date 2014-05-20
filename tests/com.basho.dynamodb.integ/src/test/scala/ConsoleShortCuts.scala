class Attributes(attributes:(String, String)*) extends acceptance.Attributes {
  for (attribute <- attributes) {
    attribute match {
      case (_name:String, _type:String) =>
        add(_name, _type)
      case _ => None
    }
  }
}
class Item extends acceptance.Item
class KeySchema extends acceptance.KeySchema

object KeyType {
  implicit def alias(key_type:KeyType.type) = acceptance.KeyType
}

object LocalSecondaryIndex {
  implicit def alias(lsi:LocalSecondaryIndex.type) = acceptance.LocalSecondaryIndex
}

object Projection {
  implicit def alias(projection:Projection.type) = acceptance.Projection
}

object ProjectionType {
  implicit def alias(projection_type:ProjectionType.type) = acceptance.ProjectionType
}

object Table {
  implicit def alias(table:Table.type) = acceptance.Table
}