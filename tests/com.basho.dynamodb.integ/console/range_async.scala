import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import com.amazonaws.services.dynamodbv2.model._

// TODO:  add await for result to converge
Table.create("books_range_bulk", "Id", "N", Some("Title"), Some("S"))

// Index Shape Controls
val t_items = 10
val per_item = 1000

val tasks: Seq[Future[Array[PutItemResult]]] =
  for (i <- 1 to t_items) yield future {
    println("Executing task " + i)
    val results: Array[PutItemResult] = Array()
    for (j <- 1 to per_item) {
      val item = new Item(
        ("Id", "N", i.toString()),
        ("Title", "S", "Title " + i.toString() + "::" + j.toString()),
        ("ISBN", "S", i.toString() + "::" + j.toString()))
      results :+ Table.put("books_range_bulk", item)
    }
    results
  }

val aggregated: Future[Seq[Array[PutItemResult]]] =
  Future.sequence(tasks)

val results: Seq[Array[PutItemResult]] =
  Await.result(aggregated, 300.seconds)

val result:QueryResult = Table.range_query("books_range_bulk", "Id", "1", Some("Title"), Some("BEGINS_WITH"), Some("Title 1"))

// TODO:  add await for result count to converge to #per_item count
// assert(per_item == result.getCount())
