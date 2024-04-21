package fr.renoux.gaston.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// scalastyle:off magic.number
class BitSetSpec extends AnyFlatSpec with Matchers {

  import BitSet.syntax._
  import BitSetSpec._

  behavior of "BitSet"
  it should "work from scratch" in {
    val bs = BitSet.from[Dog](3)(Nil)
    bs(gromit) should be(false)
    bs(lassie) should be(false)
    bs(milou) should be(false)
    an[IndexOutOfBoundsException] should be thrownBy(bs(rintintin))

    bs.nonEmpty should be(false)
    bs.actualEquals(BitSet.from[Dog](3)(Nil)) should be(true)
    bs.safeContent should be(Seq(false, false, false))

    val bsuc = bs.unsafeContent
    bsuc(0) = true
    bs(gromit) should be(true)
  }

  it should "work from a Set, with explicit count" in {
    val set = Set(gromit, milou)
    val bs = set.toBitSet(3)
    bs(gromit) should be(true)
    bs(lassie) should be(false)
    bs(milou) should be(true)
    an[IndexOutOfBoundsException] should be thrownBy(bs(rintintin))

    bs.nonEmpty should be(true)
    bs.actualEquals(set.toBitSet(3)) should be(true)
    bs.safeContent should be(Seq(true, false, true))

    val bsuc = bs.unsafeContent
    bsuc(0) = false
    bs(gromit) should be(false)
  }

  it should "work from a Set, with implicit count" in {
    implicit val cd: Count[Dog] = Count[Dog](3)
    val set = Set(gromit, milou)
    val bs = set.toBitSet
    bs(gromit) should be(true)
    bs(lassie) should be(false)
    bs(milou) should be(true)
    an[IndexOutOfBoundsException] should be thrownBy(bs(rintintin))

    bs.nonEmpty should be(true)
    bs.actualEquals(set.toBitSet(3)) should be(true)
    bs.safeContent should be(Seq(true, false, true))

    val bsuc = bs.unsafeContent
    bsuc(0) = false
    bs(gromit) should be(false)
  }

}

object BitSetSpec {
  case class Dog(id: Int, name: String) extends Identified

  case class Toy(name: String)

  val gromit: Dog = Dog(0, "Gromit")
  val lassie: Dog = Dog(1, "Lassie")
  val milou: Dog = Dog(2, "Milou")
  val rintintin: Dog = Dog(3, "Rintintin")
}
