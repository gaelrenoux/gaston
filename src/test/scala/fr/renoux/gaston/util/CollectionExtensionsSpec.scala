package fr.renoux.gaston.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CollectionExtensionsSpec extends AnyFlatSpec with Matchers {

  import CollectionExtensionsSpec.*

  "zipWith" should "work with Seqs" in {
    val result: Seq[(String, Int)] = Seq("alpha", "beta", "gamma").zipWith(_.length)
    result should be(Seq("alpha" -> 5, "beta" -> 4, "gamma" -> 5))
  }

  it should "work with Lists" in {
    val result: List[(String, Int)] = List("alpha", "beta", "gamma").zipWith(_.length)
    result should be(List("alpha" -> 5, "beta" -> 4, "gamma" -> 5))
  }

  it should "work with Sets" in {
    val result: Set[(String, Int)] = Set("alpha", "beta", "gamma").zipWith(_.length)
    result should be(Set("alpha" -> 5, "beta" -> 4, "gamma" -> 5))
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
    val result: List[Animal] = (Nil: List[Dog]).replace { case Dog("beta") => Cat("beta") }
    result should be(Nil)
  }

  it should "work on empty Set" in {
    val result: Set[Animal] = Set.empty[Dog].replace { case Dog("beta") => Cat("beta") }
    result.isEmpty should be(true)
  }

  "cross" should "work with lists" in {
    val result: List[(Int, String)] = List(1, 2, 3) x List("red", "green")
    result should be(List(1 -> "red", 1 -> "green", 2 -> "red", 2 -> "green", 3 -> "red", 3 -> "green"))
  }

  it should "work with sets" in {
    val result: Set[(Int, String)] = Set(1, 2, 3) x Set("red", "green")
    result should be(Set(1 -> "red", 1 -> "green", 2 -> "red", 2 -> "green", 3 -> "red", 3 -> "green"))
  }

  it should "work with different collections" in {
    val result1: Set[(Int, String)] = Set(1, 2, 3) x List("red", "green")
    result1 should be(Set(1 -> "red", 1 -> "green", 2 -> "red", 2 -> "green", 3 -> "red", 3 -> "green"))

    val result2: List[(Int, String)] = List(1, 2, 3) x Set("red", "green")
    result2 should be(List(1 -> "red", 1 -> "green", 2 -> "red", 2 -> "green", 3 -> "red", 3 -> "green"))
  }

  it should "work with empty on the left" in {
    val result: List[(Nothing, String)] = Nil x List("red", "green")
    result should be(Nil)
  }

  it should "work with empty on the right" in {
    val result: List[(Int, Nothing)] = List(1, 2, 3) x Nil
    result should be(Nil)
  }

  "mapWithState" should "work" in {
    val result = Seq(1, 3, 2).mapWithState("#") { (i, state) => (s"$state to $i", i.toString) }
    result._1 should be(Seq("# to 1", "1 to 3", "3 to 2"))
    result._2 should be("2")
  }

  it should "work on empty" in {
    val result = Nil.mapWithState("#") { (i, state) => (s"$state to $i", i.toString) }
    result._1 should be(Nil)
    result._2 should be("#")
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

  it should "work on empty" in {
    val seq: Seq[Either[Int, String]] = Nil
    seq.unzipEither should be(Nil, Nil)
  }

  "mapMap" should "work" in {
    val seq = Seq(Seq("apple", "orange"), Seq("kiwi"), Nil)
    seq.mapMap(_.length) should be(Seq(Seq(5, 6), Seq(4), Nil))
  }

  "mapMap" should "work on empty" in {
    val seq: Seq[Seq[String]] = Nil
    seq.mapMap(_.length) should be(Nil)
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
