## Rinamo Integration Tests
These tests use the AWS Java Client Library.

## Prerequisites
* Java 1.7
* Scala 2.10.4
* Maven 3.2.1
  * on OS X:  `brew install maven`

## Running Things
If your system has all the prerequisites, running things is easy:

* Tests
  * `mvn test`

* Test Console
  * `mvn test-compile scala:console`
  * `scala> :load console/tables.scala`
  * `scala> Table.list()`
