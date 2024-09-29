package fr.renoux.gaston.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BitMapSpec extends AnyFlatSpec with Matchers {

  import BitMapSpec._
  import BitMap.syntax._

  behavior of "BitMap"
  it should "work from scratch" in {
    val bm = BitMap.from[Dog, Toy](3, ball)(Nil)
    bm.size should be(3)
    bm(gromit) should be(ball)
    bm(lassie) should be(ball)
    bm(milou) should be(ball)
    an[IndexOutOfBoundsException] should be thrownBy(bm(rintintin))

    val bm2 = bm.map(_.name)
    bm2.size should be(3)
    bm2.apply(gromit) should be("Ball")
    bm2.apply(lassie) should be("Ball")
    bm2.apply(milou) should be("Ball")
    an[IndexOutOfBoundsException] should be thrownBy(bm2(rintintin))
  }

  it should "work from a Map, with explicit default and count" in {
    val map = Map(gromit -> rope, lassie -> stick)
    val bm = map.toBitMap(3, ball)
    bm.size should be(3)
    bm(gromit) should be(rope)
    bm(lassie) should be(stick)
    bm(milou) should be(ball)
    an[IndexOutOfBoundsException] should be thrownBy(bm(rintintin))
  }

  it should "work from a Map, with implicit count and explicit default" in {
    implicit val cd: Count[Dog] = Count[Dog](3)
    val map = Map(gromit -> rope, lassie -> stick)
    val bm = map.toBitMap(ball)
    bm.size should be(3)
    bm(gromit) should be(rope)
    bm(lassie) should be(stick)
    bm(milou) should be(ball)
    an[IndexOutOfBoundsException] should be thrownBy(bm(rintintin))
  }

  it should "work from a Map, with null default and explicit count" in {
    val map = Map(gromit -> rope, lassie -> stick)
    val bm = map.toBitMap(3)
    bm.size should be(3)
    bm(gromit) should be(rope)
    bm(lassie) should be(stick)
    bm(milou) should be(null)
    an[IndexOutOfBoundsException] should be thrownBy(bm(rintintin))
  }

  it should "work from a Map, with null default and implicit count" in {
    implicit val cd: Count[Dog] = Count[Dog](3)
    val map = Map(gromit -> rope, lassie -> stick)
    val bm = map.toBitMap
    bm.size should be(3)
    bm(gromit) should be(rope)
    bm(lassie) should be(stick)
    bm(milou) should be(null)
    an[IndexOutOfBoundsException] should be thrownBy(bm(rintintin))
  }
}

object BitMapSpec {
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
