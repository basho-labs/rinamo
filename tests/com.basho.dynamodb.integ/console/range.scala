Table.create("books_range", "Id", "N", Some("Title"), Some("S"))

val i1 = new Item(
  ("Id", "N", "101"),
  ("Title", "S", "Some Title"),
  ("ISBN", "S", "ABC"))

val i2 = new Item(
  ("Id", "N", "102"),
  ("Title", "S", "Another Title"),
  ("ISBN", "S", "DEF"))

val i3 = new Item(
  ("Id", "N", "101"),
  ("Title", "S", "Tale of Two Databases"),
  ("ISBN", "S", "XYZ"))

Table.put("books_range")(i1, i2, i3)

// Range Query
val key_conditions = new KeyConditions(
    ("Id", "N", "EQ", "101", None),
    ("Title", "S", "GT", "A", None))
Table.query("books_range", key_conditions)