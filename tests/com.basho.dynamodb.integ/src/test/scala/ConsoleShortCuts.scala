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
class LocalSecondaryIndexes(indexes:(String, KeySchema, com.amazonaws.services.dynamodbv2.model.Projection)*) extends acceptance.LocalSecondaryIndexes {
  for (index <- indexes) {
    index match {
      case (_index_name, _key_schema, _projection) =>
        add(_index_name, _key_schema, _projection)
      case _ => None
    }
  }
}

object KeyType {
  implicit def alias(key_type:KeyType.type) = acceptance.KeyType
}

object Projection {
  implicit def alias(projection:Projection.type) = acceptance.Projection
}

object ProjectionType {
  implicit def alias(projection_type:ProjectionType.type) = acceptance.ProjectionType
}

object ProvisionedThroughput {
  implicit def alias(provisioned_throughput:ProvisionedThroughput.type) = acceptance.ProvisionedThroughput
}

object Table {
  implicit def alias(table:Table.type) = acceptance.Table
}