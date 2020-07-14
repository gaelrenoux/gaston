package fr.renoux.gaston.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CollectionImplicitsSpec extends AnyFlatSpec with Matchers {

  import CollectionImplicits._

  "IterableOps.zipWith" should "work" in {
    Seq("alpha", "beta", "gamma").zipWith(_.length) should be(Seq("alpha" -> 5, "beta" -> 4, "gamma" -> 5))
  }

  it should "handle types" in {
    """val a: Seq[(String, Int)] = Seq("alpha", "beta", "gamma").zipWith(_.length)""" should compile
    """val a: List[(String, Int)] = List("alpha", "beta", "gamma").zipWith(_.length)""" should compile
    """val a: Set[(String, Int)] = Set("alpha", "beta", "gamma").zipWith(_.length)""" should compile
    """val a: Iterable[((Char, String), Int)] = Map('a' -> "alpha", 'b' -> "beta", 'c' -> "gamma").zipWith(_._2.length)""" should compile
  }
}
