package fr.renoux.gaston.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random


class RandomImplicitsSpec extends AnyFlatSpec with Matchers {

  import RandomImplicits._


  "pick" should "work for one element" in {
    val random = new Random(0) // always the same values
    random.pick(Set(1)) should be(1)
    random.pick(Set(1, 2)) should be(2)
    an[IndexOutOfBoundsException] should be thrownBy random.pick(Set.empty[Int])
  }

  it should "work for multiple elements" in {
    val random = new Random(0) // always the same values
    random.pick(Set(1), 1) should be(List(1))
    random.pick(Set(1, 2), 2) should be(List(1, 2))
    random.pick(Set(1, 2), 1) should be(List(1))
    random.pick(Set(1, 2, 3), 2) should be(List(1, 3))

    // If not enough elements, it does as much as it can
    random.pick(Set.empty[Int], 1) should be(Nil)
    random.pick(Set(1, 2), 3) should be(List(1, 2))
  }

}
