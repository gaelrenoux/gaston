package fr.renoux.gaston.util

import com.typesafe.scalalogging.Logger
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Created by gael on 07/05/17.
  */
// scalastyle:off magic.number
class CanTakeChunksSpec extends AnyFlatSpec with Matchers {
  val log = Logger[CanTakeChunksSpec]

  import CanTakeChunks._

  val testList = List("adam", "brigit", "cedric", "daniel", "edward", "fatima", "george", "hermione", "isidore", "jennifer", "kevin")

  behavior of "SeqOps.take"
  it should "return a list with as much elements as the argument list" in {
    testList.takeChunks(1, 2)._1.size should be(2)
    testList.takeChunks(1, 2, 3)._1.size should be(3)
    testList.takeChunks(1, 2, 3, 4)._1.size should be(4)
    testList.takeChunks(1, 2, 3, 4, 5)._1.size should be(5)
  }
  it should "return the correct values" in {
    testList.takeChunks(3, 5, 1) should be((List(
      List("adam", "brigit", "cedric"),
      List("daniel", "edward", "fatima", "george", "hermione"),
      List("isidore")
    ), List("jennifer", "kevin")
    ))
  }
  it should "truncate the elements once the list has been exhausted" in {
    testList.takeChunks(3, 5, 6, 2, 1) should be((List(
      List("adam", "brigit", "cedric"),
      List("daniel", "edward", "fatima", "george", "hermione"),
      List("isidore", "jennifer", "kevin"),
      Nil,
      Nil
    ), Nil))
  }

  behavior of "ListOps.takeWithRemainder"
  it should "return a filled remainder if there is one" in {
    val x: (List[List[String]], List[String]) = testList.takeChunks(List(3, 5, 1))
    x should be((List(
      List("adam", "brigit", "cedric"),
      List("daniel", "edward", "fatima", "george", "hermione"),
      List("isidore")
    ), List("jennifer", "kevin")))
  }
  it should "return an empty remainder if the result is exactly contained in elements" in {
    testList.takeChunks(List(3, 5, 3)) should be((List(
      List("adam", "brigit", "cedric"),
      List("daniel", "edward", "fatima", "george", "hermione"),
      List("isidore", "jennifer", "kevin")
    ), Nil))
  }
  it should "return an empty remainder if it is forced to truncate" in {
    testList.takeChunks(List(3, 5, 6, 2, 1)) should be((List(
      List("adam", "brigit", "cedric"),
      List("daniel", "edward", "fatima", "george", "hermione"),
      List("isidore", "jennifer", "kevin"),
      Nil,
      Nil
    ), Nil))
  }

}
