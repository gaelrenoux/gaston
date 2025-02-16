package fr.renoux.gaston.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random


class ArrayExtensionsSpec extends AnyFlatSpec with Matchers {

  "shuffle" should "return different orders values for a multi-element array" in {
    given Random = new Random(0)
    val source = (1 to 5).toArray
    source.shuffle; source should be(Array(5, 3, 2, 4, 1))
    source.shuffle; source should be(Array(4, 1, 2, 3, 5))
    source.shuffle; source should be(Array(1, 4, 2, 3, 5))
  }

  it should "return the only possibility for a single-element array" in {
    given Random = new Random(0)
    val source = Array(1)
    source.shuffle; source should be(Array(1))
    source.shuffle; source should be(Array(1))
    source.shuffle; source should be(Array(1))
  }

  it should "return an empty array when called on an empty array" in {
    given Random = new Random(0)
    val source = Array.empty[Int]
    source.shuffle; source should be( Array.empty[Int])
  }

  it should "return the same values from the same seed" in {
    def check() = {
      val array = (1 to 5).toArray
      array.shuffle(using new Random(0))
      array should be(Array(5, 3, 2, 4, 1))
    }
    check()
    check()
    check()
    check()
    check()
  }
}
