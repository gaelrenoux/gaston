package fr.renoux.gaston.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MutableArrayMapSpec extends AnyFlatSpec with Matchers {

  import MutableArrayMapSpec.*

  "MutableArrayMap.fill" should "work" in {
    val mam = MutableArrayMap.fill(3, stick)
    mam(0) should be(stick)
    mam(1) should be(stick)
    mam(2) should be(stick)
    mam.capacity should be(3)
    mam.unsafeContent should be(Array(stick, stick, stick))
  }

  it should "work with 0" in {
    val mam = MutableArrayMap.fill(0, stick)
    mam.capacity should be(0)
    mam.toArrayMap.isEmpty should be(true)
  }

  "MutableArrayMap.update" should "work" in {
    val mam = MutableArrayMap.fill(3, stick)
    mam(lassie) = ball
    mam(0) = rope
    mam(gromit) should be(rope)
    mam(lassie) should be(ball)
    mam(milou) should be(stick)
  }

}

object MutableArrayMapSpec {

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
