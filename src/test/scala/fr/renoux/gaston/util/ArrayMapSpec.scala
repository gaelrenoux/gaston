package fr.renoux.gaston.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ArrayMapSpec extends AnyFlatSpec with Matchers {

  import ArrayMap.*
  import ArrayMapSpec.*

  behavior of "ArrayMap.from"
  it should "work from no elements" in {
    val bm = ArrayMap.from[Dog, Toy](3, ball)(Nil)
    bm.capacity should be(3)
    bm.isEmpty should be(false)
    bm.nonEmpty should be(true)
    bm(gromit) should be(ball)
    bm(lassie) should be(ball)
    bm(milou) should be(ball)
    an[IndexOutOfBoundsException] should be thrownBy (bm(rintintin))
  }
  
  it should "work from some elements elements" in {
    val bm = ArrayMap.from[Dog, Toy](3, ball)(List(milou -> stick))
    bm.capacity should be(3)
    bm.isEmpty should be(false)
    bm.nonEmpty should be(true)
    bm(gromit) should be(ball)
    bm(lassie) should be(ball)
    bm(milou) should be(stick)
    an[IndexOutOfBoundsException] should be thrownBy (bm(rintintin))
  }

  behavior of "Map.toArrayMap"
  it should "work with explicit default and count" in {
    val map = Map(gromit -> rope, lassie -> stick)
    val bm = map.toArrayMap(3, ball)
    bm.capacity should be(3)
    bm.isEmpty should be(false)
    bm.nonEmpty should be(true)
    bm(gromit) should be(rope)
    bm(lassie) should be(stick)
    bm(milou) should be(ball)
    an[IndexOutOfBoundsException] should be thrownBy (bm(rintintin))
  }

  it should "work with given count and explicit default" in {
    given Count[Dog] = Count[Dog](3)
    val map = Map(gromit -> rope, lassie -> stick)
    val bm = map.toArrayMap(ball)
    bm.capacity should be(3)
    bm.isEmpty should be(false)
    bm.nonEmpty should be(true)
    bm(gromit) should be(rope)
    bm(lassie) should be(stick)
    bm(milou) should be(ball)
    an[IndexOutOfBoundsException] should be thrownBy (bm(rintintin))
  }

  it should "work with null default and explicit count" in {
    val map = Map(gromit -> rope, lassie -> stick)
    val bm = map.toArrayMap(3)
    bm.capacity should be(3)
    bm.isEmpty should be(false)
    bm.nonEmpty should be(true)
    bm(gromit) should be(rope)
    bm(lassie) should be(stick)
    bm(milou) should be(null)
    an[IndexOutOfBoundsException] should be thrownBy (bm(rintintin))
  }

  it should "work with null default and given count" in {
    given Count[Dog] = Count[Dog](3)
    val map = Map(gromit -> rope, lassie -> stick)
    val bm = map.toArrayMap
    bm.capacity should be(3)
    bm.isEmpty should be(false)
    bm.nonEmpty should be(true)
    bm(gromit) should be(rope)
    bm(lassie) should be(stick)
    bm(milou) should be(null)
    an[IndexOutOfBoundsException] should be thrownBy (bm(rintintin))
  }
  
  "map" should "work" in {
    val bm = ArrayMap.from[Dog, Toy](3, ball)(List(gromit -> ball, lassie -> stick, milou -> rope))

    val bm2 = bm.map(_.name)
    bm2.capacity should be(3)
    bm2.isEmpty should be(false)
    bm2.nonEmpty should be(true)
    bm2.apply(gromit) should be("Ball")
    bm2.apply(lassie) should be("Stick")
    bm2.apply(milou) should be("Rope")
    an[IndexOutOfBoundsException] should be thrownBy (bm2(rintintin))
  }
}

object ArrayMapSpec {
  case class Dog(id: Int, name: String) extends Identified

  case class Toy(name: String)

  val gromit: Dog = Dog(0, "Gromit")
  val lassie: Dog = Dog(1, "Lassie")
  val milou: Dog = Dog(2, "Milou")
  val rintintin: Dog = Dog(3, "Rintintin")

  val ball: Toy = Toy("Ball")
  val stick: Toy = Toy("Stick")
  val rope: Toy = Toy("Rope")
}
