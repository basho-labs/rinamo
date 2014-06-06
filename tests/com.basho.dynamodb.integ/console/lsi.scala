// Create a table using LSI
val table_attributes = new Attributes(
  ("Artist", "S"),
  ("SongTitle", "S"),
  ("AlbumTitle", "S")
)

val tableKey = new KeySchema(
  ("Artist", KeyType.HASH),
  ("SongTitle", KeyType.RANGE))

val indexKey = new KeySchema(
  ("Artist", KeyType.HASH),
  ("AlbumTitle", KeyType.RANGE))

val projection = Projection.build_value(ProjectionType.INCLUDE, Some(List("Genre", "Year")))

val secondary_indexes = new LocalSecondaryIndexes (
  ("AlbumTitleIndex", indexKey, projection)
)

val provisioned_throughput = ProvisionedThroughput.build_value(1L, 1L)

val table_result = Table.create("LSI_Test_Table", tableKey, table_attributes, Some(secondary_indexes), None, provisioned_throughput)

// create and put items
val i1 = new Item(
  ("Artist", "S", "Air Supply"),
  ("SongTitle", "S", "All Out of Love"),
  ("AlbumTitle", "S", "Ultimate Air Supply"),
  ("Genre", "S", "Pop"),
  ("Year", "S", "1990"))

val i2 = new Item(
  ("Artist", "S", "Enya"),
  ("SongTitle", "S", "Anywhere Is"),
  ("AlbumTitle", "S", "The Very Best of Enya"),
  ("Genre", "S", "Pop"),
  ("Year", "S", "2009"))

val i3 = new Item(
  ("Artist", "S", "London Symphony Orchestra"),
  ("SongTitle", "S", "Ode To Joy"),
  ("AlbumTitle", "S", "Various Artists"),
  ("Genre", "S", "Holiday"),
  ("Year", "S", "2009"))

val i4 = new Item(
  ("Artist", "S", "Enya"),
  ("SongTitle", "S", "Only Time"),
  ("AlbumTitle", "S", "The Very Best of Enya"),
  ("Genre", "S", "Pop"),
  ("Year", "S", "2009"))

Table.put("LSI_Test_Table")(i1, i2, i3, i4)

// Range Query (returns 1 result)
val r_key_conditions = new KeyConditions(
  ("Artist", "S", "EQ", "Enya", None),
  ("SongTitle", "S", "GT", "O", None))
Table.query("LSI_Test_Table", r_key_conditions)

// LSI Query (returns 2 results)
val l_key_conditions = new KeyConditions(
  ("Artist", "S", "EQ", "Enya", None),
  ("AlbumTitle", "S", "GT", "A", None))
Table.query("LSI_Test_Table", l_key_conditions, Some("AlbumTitleIndex"))
