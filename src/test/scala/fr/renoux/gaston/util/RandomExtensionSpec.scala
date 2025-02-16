package fr.renoux.gaston.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random


class RandomExtensionSpec extends AnyFlatSpec with Matchers {

  "pick (one)" should "return several values from a multi-element set" in {
    val random = new Random(0)
    val source = Set.tabulate(100)(identity)
    random.pick(source) should be(98)
    random.pick(source) should be(44)
    random.pick(source) should be(28)
  }

  it should "return the only value from a single-element set" in {
    val random = new Random(0)
    val source = Set(1)
    random.pick(source) should be(1)
    random.pick(source) should be(1)
    random.pick(source) should be(1)
  }

  it should "throw an exception on an empty set" in {
    val random = new Random(0)
    val source = Set.empty[Int]
    an[IndexOutOfBoundsException] should be thrownBy random.pick(source)
  }

  it should "return the same value from the same seed" in {
    val source = Set.tabulate(100)(identity)
    Random(0).pick(source) should be(98)
    Random(0).pick(source) should be(98)
    Random(0).pick(source) should be(98)
    Random(0).pick(source) should be(98)
    Random(0).pick(source) should be(98)
  }

  "pick (multiple)" should "return several values from a multi-element set" in {
    val random = new Random(0) // always the same values
    val source = Set.tabulate(100)(identity)
    random.pick(source, 1) should contain theSameElementsInOrderAs List(60)
    random.pick(source, 1) should contain theSameElementsInOrderAs List(36)
    random.pick(source, 5) should contain theSameElementsInOrderAs List(27, 77, 90, 80, 69)
    random.pick(source, 5) should contain theSameElementsInOrderAs List(1, 39, 37, 32, 35)
  }

  it should "return as many elements as possible from a smaller set" in {
    val random = new Random(0) // always the same values
    random.pick(Set.empty[Int], 1) should be(Nil)
    val source = Set.tabulate(3)(identity)
    random.pick(source, 5) should contain theSameElementsInOrderAs List(2, 1, 0)
    random.pick(source, 5) should contain theSameElementsInOrderAs List(0, 2, 1)
  }

  it should "never return the same element twice" in {
    val random = new Random(0) // always the same values
    val source = Set.tabulate(5)(identity)
    for (i <- 0 until 1000) {
      random.pick(source, 2).toSet.size should be(2)
    }
  }

  it should "return the same values from the same seed" in {
    val source = Set.tabulate(100)(identity)
    Random(0).pick(source, 5) should contain theSameElementsInOrderAs List(60, 49, 47, 26, 22)
    Random(0).pick(source, 5) should contain theSameElementsInOrderAs List(60, 49, 47, 26, 22)
    Random(0).pick(source, 5) should contain theSameElementsInOrderAs List(60, 49, 47, 26, 22)
    Random(0).pick(source, 5) should contain theSameElementsInOrderAs List(60, 49, 47, 26, 22)
    Random(0).pick(source, 5) should contain theSameElementsInOrderAs List(60, 49, 47, 26, 22)
  }
}
