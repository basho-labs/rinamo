Table.create(table_name = "my_new_table", key_name = "VideoId", rc = 10L, wc = 5L)
Table.list()
Table.describe("my_new_table")
Table.delete("my_new_table")
