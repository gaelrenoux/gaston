package fr.renoux.gaston.util

import com.typesafe.scalalogging.Logger
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by gael on 07/05/17.
  */
class DispatchSpec extends FlatSpec with Matchers {
  val log = Logger[DispatchSpec]

  val TestList = List("adam", "brigit", "cedric", "daniel", "edward", "fatima", "george", "hermione", "isidore", "jennifer", "kevin")

  behavior of "Dispatch.equally"
  it should "dispatch all elements equally when no maxes are given" in {
    Dispatch.equally(3)(TestList) should be(List(
      List("adam", "brigit", "cedric", "daniel"),
      List("edward", "fatima", "george", "hermione"),
      List("isidore", "jennifer", "kevin")
    ))
  }

  behavior of "Dispatch.equallyWithMaxes"
  it should "dispatch all elements according to maxes" in {
    Dispatch.equallyWithMaxes(Seq(2, 8, 8))(TestList) should be((List(
      List("adam", "brigit"),
      List("cedric", "daniel", "edward", "fatima", "george"),
      List("hermione", "isidore", "jennifer", "kevin")
    ), Nil))
  }
  it should "have a remainder if it can't put everything" in {
    Dispatch.equallyWithMaxes(Seq(2, 3, 2))(TestList) should be((List(
      List("adam", "brigit"),
      List("cedric", "daniel", "edward"),
      List("fatima", "george")
    ), List("hermione", "isidore", "jennifer", "kevin")))
  }
}