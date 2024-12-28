package fr.renoux.gaston.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CollectionExtensionsSpec extends AnyFlatSpec with Matchers {

  import CollectionExtensionsSpec.*

  "zipWith" should "work" in {
    Seq("alpha", "beta", "gamma").zipWith(_.length) should be(Seq("alpha" -> 5, "beta" -> 4, "gamma" -> 5))
  }

  it should "handle types" in {
    """val a: Seq[(String, Int)] = Seq("alpha", "beta", "gamma").zipWith(_.length)""" should compile
    """val a: List[(String, Int)] = List("alpha", "beta", "gamma").zipWith(_.length)""" should compile
    """val a: Set[(String, Int)] = Set("alpha", "beta", "gamma").zipWith(_.length)""" should compile
    // TODO The whole extension needs to be reworked for the next one
    // """val a: Iterable[((Char, String), Int)] = Map('a' -> "alpha", 'b' -> "beta", 'c' -> "gamma").zipWith(_._2.length)""" should compile
    println(a)
  }

  "replace" should "work on Seqs" in {
    val result: Seq[Animal] = Seq(Dog("alpha"), Dog("beta"), Dog("gamma")).replace { case Dog("beta") => Cat("beta") }
    result should be(Seq(Dog("alpha"), Cat("beta"), Dog("gamma")))
  }

  it should "work on Sets" in {
    val result: Set[Animal] = Set(Dog("alpha"), Dog("beta"), Dog("gamma")).replace { case Dog("beta") => Cat("beta") }
    result should be(Set(Dog("alpha"), Cat("beta"), Dog("gamma")))
  }

  it should "work on Nil" in {
    val result: List[Animal] = List.empty[Dog].replace { case Dog("beta") => Cat("beta") }
    result should be(Nil)
  }

  "cross" should "work" in {
    val result = List(1, 2, 3) x List("red", "green")
    result should be(List(1 -> "red", 1 -> "green", 2 -> "red", 2 -> "green", 3 -> "red", 3 -> "green"))
  }

  "mapWithState" should "work" in {
    val result = Seq(1, 3, 2).mapWithState("#") { (i, state) => (s"$state to $i", i.toString) }
    result._1 should be(Seq("# to 1", "1 to 3", "3 to 2"))
    result._2 should be("2")
  }

  "filterMinBy" should "work" in {
    val result = Seq("apple", "banana", "orange", "lemon").filterMinBy(_.length)
    result should be(Seq("apple", "lemon"))
  }

  it should "work on empty" in {
    List.empty[String].filterMinBy(_.length) should be(Nil)
  }

  "unzipEither" should "work on Seq" in {
    val seq = Seq(Left(1), Right("Hello"), Right("World"), Left(42))
    seq.unzipEither should be(Seq(1, 42), Seq("Hello", "World"))
  }

  it should "work on Set" in {
    val set = Set(Left(1), Right("Hello"), Right("World"), Left(42))
    set.unzipEither should be(Set(1, 42), Set("Hello", "World"))
  }

  "mapMap" should "work" in {
    val seq = Seq(Seq("apple", "orange"), Seq("kiwi"), Nil)
    seq.mapMap(_.length) should be(Seq(Seq(5, 6), Seq(4), Nil))
  }

  "getMinKey" should "work" in {
    Map(3 -> "orange", 2 -> "kiwi", 8 -> "tomato").minKeyOption should be(Some("kiwi"))
  }

  "getMinKey" should "work on empty Map" in {
    Map.empty[String, Dog].minKeyOption should be(None)
  }

  "updatedWithOrElse" should "work to replace a value" in {
    Map(3 -> "orange", 2 -> "kiwi", 8 -> "tomato").updateAtKeyOrElse(8)(f => f + f, "apple") should
      be(Map(3 -> "orange", 2 -> "kiwi", 8 -> "tomatotomato"))
  }

  it should "work to add a value" in {
    Map(3 -> "orange", 2 -> "kiwi", 8 -> "tomato").updateAtKeyOrElse(9)(f => f + f, "apple") should
      be(Map(3 -> "orange", 2 -> "kiwi", 8 -> "tomato", 9 -> "apple"))
  }

  "mapValuesStrict" should "work" in {
    Map(3 -> "orange", 2 -> "kiwi", 8 -> "tomato").mapValuesStrict(_.length) should be(Map(3 -> 6, 2 -> 4, 8 -> 6))
  }

  it should "not be lazy" in {
    var count = 0

    def transform(str: String): Int = {
      count += 1
      str.length
    }

    val map = Map(3 -> "orange", 2 -> "kiwi", 8 -> "tomato")
    val _ = map.view.mapValues(transform) // basic method in Scala, checks it doesn't change the count
    count should be(0)
    val result = map.mapValuesStrict(transform)
    count should be(3)
    result should be(Map(3 -> 6, 2 -> 4, 8 -> 6))
  }

  "zipByKeys" should "work" in {
    val m1 = Map(1 -> "orange", 5 -> "kiwi")
    val m2 = Map(5 -> "tomato", 7 -> "apple", 11 -> "nut")
    m1.zipByKeys(m2) should be(Map(
      1 -> (Some("orange"), None),
      5 -> (Some("kiwi"), Some("tomato")),
      7 -> (None, Some("apple")),
      11 -> (None, Some("nut"))
    ))
  }

  it should "work with an empty map on the left" in {
    val m = Map(1 -> "orange", 5 -> "kiwi")
    m.zipByKeys(Map.empty) should be(Map(
      1 -> (Some("orange"), None),
      5 -> (Some("kiwi"), None)
    ))
  }

  it should "work with an empty map on the right" in {
    val m = Map(1 -> "orange", 5 -> "kiwi")
    Map.empty.zipByKeys(m) should be(Map(
      1 -> (None, Some("orange")),
      5 -> (None, Some("kiwi"))
    ))
  }

  it should "work with two empty maps" in {
    Map.empty.zipByKeys(Map.empty) should be(Map.empty)
  }

  "develop" should "work in the nominal case on a Seq" in {
    val s = Seq("abc", "d", "ef")
    s.develop(_.toSeq) should be(Seq(
      Seq('a', 'd', 'e'),
      Seq('a', 'd', 'f'),
      Seq('b', 'd', 'e'),
      Seq('b', 'd', 'f'),
      Seq('c', 'd', 'e'),
      Seq('c', 'd', 'f')
    ))
  }

  "develop" should "work in the nominal case on a Set" in {
    val s = Set("abc", "d", "ef")
    s.develop(_.toSet) should be(Set(
      Set('a', 'd', 'e'),
      Set('a', 'd', 'f'),
      Set('b', 'd', 'e'),
      Set('b', 'd', 'f'),
      Set('c', 'd', 'e'),
      Set('c', 'd', 'f')
    ))
  }
}

object CollectionExtensionsSpec {
  sealed trait Animal

  case class Dog(name: String) extends Animal

  case class Cat(name: String) extends Animal
}
