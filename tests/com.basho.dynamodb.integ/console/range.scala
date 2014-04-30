Table.create("books_range", "Id", "N", Some("Title"), Some("S"))

val i1 = new Item()
i1.add("Id", "N", "101")
i1.add("Title", "S", "Some Title")

val i2 = new Item()
i2.add("Id", "N", "102")
i2.add("Title", "S", "Another Title")

val i3 = new Item()
i3.add("Id", "N", "101")
i3.add("Title", "S", "Tale of Two Databases")

Table.put("books_range", i1)
Table.put("books_range", i2)
Table.put("books_range", i3)

Table.query("books_range", "Id", "101", Some("Title"), Some("GT"), Some("A"))

Table.scan("books_range")