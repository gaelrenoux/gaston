package fr.renoux.gaston.util

import com.typesafe.scalalogging.Logger
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by gael on 07/05/17.
  */
class CollectionImplicitsSpec extends FlatSpec with Matchers {
  val log = Logger[CollectionImplicitsSpec]

  import CollectionImplicits._

  val TestList = List("adam", "brigit", "cedric", "daniel", "edward", "fatima", "george", "hermione", "isidore", "jennifer", "kevin")

  behavior of "ListOps.take"
  it should "return a list with as much elements as the argument list" in {
    TestList.take(1, 2).size should be(2)
    TestList.take(1, 2, 3).size should be(3)
    TestList.take(1, 2, 3, 4).size should be(4)
    TestList.take(1, 2, 3, 4, 5).size should be(5)
  }
  it should "return the correct values" in {
    TestList.take(3, 5, 1) should be(List(
      List("adam", "brigit", "cedric"),
      List("daniel", "edward", "fatima", "george", "hermione"),
      List("isidore")
    ))
  }
  it should "truncate the elements once the list has been exhausted" in {
    TestList.take(3, 5, 6, 2, 1) should be(List(
      List("adam", "brigit", "cedric"),
      List("daniel", "edward", "fatima", "george", "hermione"),
      List("isidore", "jennifer", "kevin"),
      Nil,
      Nil
    ))
  }

  behavior of "ListOps.takeWithRemainder"
  it should "return a filled remainder if there is one" in {
    TestList.takeWithRemainder(List(3, 5, 1)) should be((List(
      List("adam", "brigit", "cedric"),
      List("daniel", "edward", "fatima", "george", "hermione"),
      List("isidore")
    ), List("jennifer", "kevin")))
  }
  it should "return an empty remainder if the result is exactly contained in elements" in {
    TestList.takeWithRemainder(List(3, 5, 3)) should be((List(
      List("adam", "brigit", "cedric"),
      List("daniel", "edward", "fatima", "george", "hermione"),
      List("isidore", "jennifer", "kevin")
    ), Nil))
  }
  it should "return an empty remainder if it is forced to truncate" in {
    TestList.takeWithRemainder(List(3, 5, 6, 2, 1)) should be((List(
      List("adam", "brigit", "cedric"),
      List("daniel", "edward", "fatima", "george", "hermione"),
      List("isidore", "jennifer", "kevin"),
      Nil,
      Nil
    ), Nil))
  }

}