package fr.renoux.gaston.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ArraySetSpec extends AnyFlatSpec with Matchers {

  import ArraySet.*
  import ArraySetSpec.*

  behavior of "ArraySet.from"
  it should "work from no elements" in {
    val bs = ArraySet.from[Dog](3)(Nil)
    bs(gromit) should be(false)
    bs(lassie) should be(false)
    bs(milou) should be(false)
    an[IndexOutOfBoundsException] should be thrownBy (bs(rintintin))

    bs.nonEmpty should be(false)
    bs.actualEquals(ArraySet.from[Dog](3)(Nil)) should be(true)
    bs.safeContent should be(Seq(false, false, false))
    bs.toIdSet should be(Set.empty)
    bs.toGoodString should be("[]")

    val bsuc = bs.unsafeContent
    bsuc(0) = true
    bs(gromit) should be(true)
  }

  it should "work from some elements" in {
    val bs = ArraySet.from[Dog](3)(List(lassie, milou))
    bs(gromit) should be(false)
    bs(lassie) should be(true)
    bs(milou) should be(true)
    an[IndexOutOfBoundsException] should be thrownBy (bs(rintintin))

    bs.nonEmpty should be(true)
    bs.actualEquals(ArraySet.from[Dog](3)(List(lassie, milou))) should be(true)
    bs.safeContent should be(Seq(false, true, true))
    bs.toIdSet should be(Set(lassie.id, milou.id))
    bs.toGoodString should be("[1, 2]")
  }

  behavior of "Set.toArraySet"
  it should "work with explicit count" in {
    val set = Set(gromit, milou)
    val bs = set.toArraySet(3)
    bs(gromit) should be(true)
    bs(lassie) should be(false)
    bs(milou) should be(true)
    an[IndexOutOfBoundsException] should be thrownBy (bs(rintintin))

    bs.nonEmpty should be(true)
    bs.actualEquals(set.toArraySet(3)) should be(true)
    bs.safeContent should be(Seq(true, false, true))
    bs.toIdSet should be(Set(gromit.id, milou.id))
    bs.toGoodString should be("[0, 2]")

    val bsuc = bs.unsafeContent
    bsuc(0) = false
    bs(gromit) should be(false)
  }

  it should "work with given count" in {
    given Count[Dog] = Count[Dog](3)
    val set = Set(gromit, milou)
    val bs = set.toArraySet
    bs(gromit) should be(true)
    bs(lassie) should be(false)
    bs(milou) should be(true)
    an[IndexOutOfBoundsException] should be thrownBy (bs(rintintin))

    bs.nonEmpty should be(true)
    bs.actualEquals(set.toArraySet(3)) should be(true)
    bs.safeContent should be(Seq(true, false, true))
    bs.toIdSet should be(Set(gromit.id, milou.id))
    bs.toGoodString should be("[0, 2]")

    val bsuc = bs.unsafeContent
    bsuc(0) = false
    bs(gromit) should be(false)
  }

  "empty" should "create an empty ArraySet" in {
    given Count[Dog] = Count[Dog](3)
    val bs = ArraySet.empty[Dog]
    bs(gromit) should be(false)
    bs(lassie) should be(false)
    bs(milou) should be(false)
  }

  "countIntersection" should "work" in {
    given Count[Dog] = Count[Dog](3)
    val set1 = Set(gromit, milou).toArraySet
    val set2 = Set(lassie, gromit).toArraySet
    set1.countIntersection(set2) should be(1)
  }

  it should "work with empty" in {
    given Count[Dog] = Count[Dog](3)
    val set1 = Set(gromit, milou).toArraySet
    val empty = ArraySet.empty[Dog]
    set1.countIntersection(empty) should be(0)
    empty.countIntersection(set1) should be(0)
  }

}

object ArraySetSpec {
  case class Dog(id: Int, name: String) extends Identified

  case class Toy(name: String)

  val gromit: Dog = Dog(0, "Gromit")
  val lassie: Dog = Dog(1, "Lassie")
  val milou: Dog = Dog(2, "Milou")
  val rintintin: Dog = Dog(3, "Rintintin")
}
