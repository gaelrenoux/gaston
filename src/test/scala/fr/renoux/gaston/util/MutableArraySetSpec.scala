package fr.renoux.gaston.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MutableArraySetSpec extends AnyFlatSpec with Matchers {

  import MutableArraySetSpec.*

  "MutableArraySet.full" should "work" in {
    val mas = MutableArraySet.full(3)
    mas.capacity should be(3)
    mas.size should be(3)
    mas.isEmpty should be(false)
    mas.nonEmpty should be(true)

    mas.toIdSet should be (Set(0, 1, 2))
    mas(gromit) should be(true)
    mas(lassie) should be(true)
    mas(milou) should be(true)
  }

  "MutableArraySet.empty" should "work" in {
    val mas = MutableArraySet.empty(3)
    mas.capacity should be(3)
    mas.size should be(0)
    mas.isEmpty should be(true)
    mas.nonEmpty should be(false)

    mas.toIdSet should be (Set.empty)
    mas(gromit) should be(false)
    mas(lassie) should be(false)
    mas(milou) should be(false)
  }

  "MutableArraySet.add" should "work" in {
    val mas = MutableArraySet.empty(3)
    mas.add(milou)
    mas.add(gromit)

    mas.size should be(2)
    mas.toIdSet should be (Set(0, 2))
    mas(gromit) should be(true)
    mas(lassie) should be(false)
    mas(milou) should be(true)
  }

  "MutableArraySet.addAll" should "work" in {
    val mas = MutableArraySet.empty(3)
    mas.addAll(List(milou, gromit))

    mas.size should be(2)
    mas.toIdSet should be (Set(0, 2))
    mas(gromit) should be(true)
    mas(lassie) should be(false)
    mas(milou) should be(true)
  }

  "MutableArraySet.remove" should "work" in {
    val mas = MutableArraySet.full(3)
    mas.remove(milou)
    mas.remove(gromit)

    println(mas.toGoodString)
    mas.size should be(1)
    mas.toIdSet should be (Set(1))
    mas(gromit) should be(false)
    mas(lassie) should be(true)
    mas(milou) should be(false)
  }

  "MutableArraySet.foreachId" should "work" in {
    val mas = MutableArraySet.empty(3)
    mas.add(milou)
    mas.add(gromit)

    var acc = ""
    mas.foreachId { id => acc = acc + id }
    acc should be ("02")
  }

  it should "work on empty" in {
    val mas = MutableArraySet.empty(3)

    var acc = ""
    mas.foreachId { id => acc = acc + id }
    acc should be ("")
  }

  "toGoodString" should "work" in {
    val mas = MutableArraySet.empty(3)
    mas.add(milou)
    mas.add(gromit)

    mas.toGoodString should be("[0, 2]")
  }

}

object MutableArraySetSpec {

  case class Dog(id: Int, name: String) extends Identified

  val gromit: Dog = Dog(0, "Gromit")
  val lassie: Dog = Dog(1, "Lassie")
  val milou: Dog = Dog(2, "Milou")
  val rintintin: Dog = Dog(3, "Rintintin")

}
