// Create a table using LSI
val table_attributes = new Attributes(
  ("Artist", "S"),
  ("SongTitle", "S"),
  ("AlbumTitle", "S")
)

val tableKey = new KeySchema()
tableKey.add("Artist", KeyType.HASH)
tableKey.add("SongTitle", KeyType.RANGE)

val indexKey = new KeySchema()
indexKey.add("Artist", KeyType.HASH)
indexKey.add("AlbumTitle", KeyType.RANGE)

val projection = Projection.build_value(ProjectionType.INCLUDE, List("Genre", "Year"))

val secondary_indexes = new LocalSecondaryIndexes (
  ("AlbumTitleIndex", indexKey, projection)
)

val provisioned_throughput = ProvisionedThroughput.build_value(1L, 1L)

val table_result = Table.create("LSI_Test_Table", tableKey, table_attributes, secondary_indexes, provisioned_throughput)

// create and put items
val i1 = new Item()
i1.add("Artist", "S", "Air Supply")
i1.add("SongTitle", "S", "All Out of Love")
i1.add("AlbumTitle", "S", "Ultimate Air Supply")
i1.add("Genre", "S", "Pop")
i1.add("Year", "S", "1990")

val i2 = new Item()
i2.add("Artist", "S", "Enya")
i2.add("SongTitle", "S", "Anywhere Is")
i2.add("AlbumTitle", "S", "The Very Best of Enya")
i2.add("Genre", "S", "Pop")
i2.add("Year", "S", "2009")

val i3 = new Item()
i3.add("Artist", "S", "London Symphony Orchestra")
i3.add("SongTitle", "S", "Ode To Joy")
i3.add("AlbumTitle", "S", "Various Artists")
i3.add("Genre", "S", "Holiday")
i3.add("Year", "S", "2009")

Table.put("LSI_Test_Table", i1)
Table.put("LSI_Test_Table", i2)
Table.put("LSI_Test_Table", i3)

// need a new LSI & GSI query test interface
Table.query("LSI_Test_Table", "Artist", "Air Supply", Some("AlbumTitle"), Some("GT"), Some("A"))
