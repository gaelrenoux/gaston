package fr.renoux.gaston.tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class CombinerAnonymizerSpec extends AnyFlatSpec with Matchers {

  "First pass" should "work" in {
    val anon = new CombinerAnonymizer(0, List("green", "red", "blue"), List("bird", "fish", "lizard"))

    val one = anon.anonymized("one")
    val two = anon.anonymized("two")
    val three = anon.anonymized("three")
    println(s"$one / $two / $three")

    one shouldNot be(two)
    one shouldNot be(three)
    two shouldNot be(three)

    val Array(one1, one2) = one.split(" ")
    val Array(two1, two2) = two.split(" ")
    val Array(three1, three2) = three.split(" ")

    one1 shouldNot be(two1)
    one1 shouldNot be(three1)
    two1 shouldNot be(three1)

    one2 shouldNot be(two2)
    one2 shouldNot be(three2)
    two2 shouldNot be(three2)

  }

  "Second pass" should "work" in {
    val anon = new CombinerAnonymizer(0, List("green", "red", "blue"), List("bird", "fish", "lizard"))

    val initial = List.tabulate(9)(identity).map(_.toString)
    val modified = initial.map(anon.anonymized)
    println(modified)
    modified.toSet.size should be(modified.size)
  }

  "Third pass" should "work" in {
    val anon = new CombinerAnonymizer(0, List("green", "red", "blue"), List("bird", "fish", "lizard"))

    val initial = List.tabulate(30)(identity).map(_.toString)
    val modified = initial.map(anon.anonymized)
    println(modified)
    modified.toSet.size should be(modified.size)
  }

}
