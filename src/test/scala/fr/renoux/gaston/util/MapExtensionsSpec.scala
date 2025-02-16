package fr.renoux.gaston.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MapExtensionsSpec extends AnyFlatSpec with Matchers {

  "getMinKey" should "work" in {
    Map(3 -> "orange", 2 -> "kiwi", 8 -> "tomato").minKeyOption should be(Some("kiwi"))
  }

  it should "work on empty" in {
    Map.empty[Int, String].minKeyOption should be(None)
  }

  "updatedAtKeyOrElse" should "work to replace a value" in {
    Map(3 -> "orange", 2 -> "kiwi", 8 -> "tomato").updatedAtKeyOrElse(8)(f => f + f, "apple") should
      be(Map(3 -> "orange", 2 -> "kiwi", 8 -> "tomatotomato"))
  }

  it should "work to add a value" in {
    Map(3 -> "orange", 2 -> "kiwi", 8 -> "tomato").updatedAtKeyOrElse(9)(f => f + f, "apple") should
      be(Map(3 -> "orange", 2 -> "kiwi", 8 -> "tomato", 9 -> "apple"))
  }

  it should "work on empty" in {
    Map.empty[Int, String].updatedAtKeyOrElse(9)(f => f + f, "apple") should
      be(Map(9 -> "apple"))
  }

  "mapValuesStrict" should "work" in {
    Map(3 -> "orange", 2 -> "kiwi", 8 -> "tomato").mapValuesStrict(_.length) should be(Map(3 -> 6, 2 -> 4, 8 -> 6))
  }

  it should "work on empty" in {
    Map.empty[Int, String].mapValuesStrict(_.length) should be(Map.empty[Int, Int])
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

  "toFormattedString" should "work" in {
    val m = Map(5 -> "tomato", 7 -> "apple", 11 -> "nut")
    m.toFormattedString should be(
      """5: tomato
        |7: apple
        |11: nut""".stripMargin)
  }
}

