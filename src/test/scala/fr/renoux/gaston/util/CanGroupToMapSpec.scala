package fr.renoux.gaston.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CanGroupToMapSpec extends AnyFlatSpec with Matchers {

  import CanGroupToMap.given
  import CanGroupToMapSpec.*

  behavior of "List"
  it should "work on a non-empty List" in {
    val list: List[(String, Fruit)] = List("green" -> Kiwi, "orange" -> Orange, "green" -> Kiwi, "green" -> Avocado)
    list.groupToMap should be(Map("green" -> List(Kiwi, Kiwi, Avocado), "orange" -> List(Orange)))
  }
  it should "work on Nil" in {
    val list: List[(String, Fruit)] = Nil
    list.groupToMap should be(Map.empty)
  }

  behavior of "Seq"
  it should "work on a non-empty Seq" in {
    val seq: Seq[(String, Fruit)] = Seq("green" -> Kiwi, "orange" -> Orange, "green" -> Kiwi, "green" -> Avocado)
    seq.groupToMap should be(Map("green" -> Seq(Kiwi, Kiwi, Avocado), "orange" -> Seq(Orange)))
  }
  it should "work on Seq.empty" in {
    val seq: Seq[(String, Fruit)] = Seq.empty
    seq.groupToMap should be(Map.empty)
  }

  behavior of "Set"
  it should "work on a non-empty Set" in {
    val set: Set[(String, Fruit)] = Set("green" -> Kiwi, "orange" -> Orange, "green" -> Kiwi, "green" -> Avocado)
    set.groupToMap should be(Map("green" -> Set(Kiwi, Avocado), "orange" -> Set(Orange)))
  }
  it should "work on Set.empty" in {
    val set: Set[(String, Fruit)] = Set.empty
    set.groupToMap should be(Map.empty)
  }

  behavior of "Iterable"
  it should "work on a non-empty Iterable" in {
    val it: Iterable[(String, Fruit)] = Iterable("green" -> Kiwi, "orange" -> Orange, "green" -> Kiwi, "green" -> Avocado)
    it.groupToMap should be(Map("green" -> Iterable(Kiwi, Kiwi, Avocado), "orange" -> Iterable(Orange)))
  }
  it should "work on Iterable.empty" in {
    val it: Iterable[(String, Fruit)] = Iterable.empty
    it.groupToMap should be(Map.empty)
  }

  behavior of "Seq (triplets)"
  it should "work on a non-empty Seq" in {
    val seq: Seq[(String, Fruit, Int)] = Seq(("green", Kiwi, 1), ("orange", Orange, 2), ("green", Kiwi, 2), ("green", Avocado, 1))
    seq.groupToMap should be(Map("green" -> Map(Kiwi -> Seq(1, 2), Avocado -> Seq(1)), "orange" -> Map(Orange -> Seq(2))))
  }
  it should "work on Seq.empty" in {
    val seq: Seq[(String, Fruit, Int)] = Seq.empty
    seq.groupToMap should be(Map.empty)
  }
}

object CanGroupToMapSpec {
  sealed trait Fruit

  case object Kiwi extends Fruit

  case object Orange extends Fruit

  case object Lemon extends Fruit

  case object Avocado extends Fruit
}
