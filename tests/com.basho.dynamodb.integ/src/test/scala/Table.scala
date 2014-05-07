object Table {
  implicit def alias(table:Table.type) = acceptance.Table
}