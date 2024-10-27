package fr.renoux.gaston.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class TupleImplicitsSpec extends AnyFlatSpec with Matchers {

  import TupleImplicits.*

  "map1" should "work" in {
    ("Hello", "Woooooorld").map1(_.length) should be((5, "Woooooorld"))
  }

  "map2" should "work" in {
    ("Hello", "Woooooorld").map2(_.length) should be(("Hello", 10))
  }

}
