Table.create("books_hash", "Id", "N")

val i1 = new Item(
  ("Id", "N", "101"),
  ("Title", "S", "Some Title"))

val i2 = new Item(
  ("Id", "N", "102"),
  ("Title", "S", "Another Title"))

val i3 = new Item(
  ("Id", "N", "101"),
  ("Title", "S", "Tale of Two Databases"))

Table.put("books_hash")(i1, i2, i3)

Table.get("books_hash", "Id", "101", "N")
