package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase

import scala.util.Random

class IdTest extends TestBase {

  given rand: Random = new Random(0)

  val sidPositive: SlotId = 8
  val sidZero: SlotId = 0
  val sidNegative: SlotId = -3

  "basics" - {

    "isNatural" in {
      sidPositive.isNatural should be(true)
      sidZero.isNatural should be(true)
      sidNegative.isNatural should be(false)
    }

    "toPrettyString" in {
      sidPositive.toPrettyString should be("8")
      sidZero.toPrettyString should be("0")
      sidNegative.toPrettyString should be("-3")
    }
  }

  "ordering" - {
    "work on the general Id type" in {
      val sids: Seq[Id] = Seq.fill(10)(rand.nextInt(64))
      val sortedSeq = sids.sorted
      sortedSeq should be(sorted)
    }
    
    "works on a specific id type" in {
      val sids: Seq[SlotId] = Seq.fill(10)(rand.nextInt(64))
      val sortedSeq = sids.sorted
      sortedSeq should be(sorted)
    }
  }

  "next" - {
    given CountAll[SlotId] = CountAll(12)

    "on increase" in {
      sidPositive.next.value should be(9)
      sidZero.next.value should be(1)
      sidNegative.next.value should be(-2)
    }

    "on rollover" in {
      val sidMax: SlotId = 11
      sidMax.next.value should be(0)
    }

    "already too high" in {
      val sidHigh: SlotId = 15
      sidHigh.next.value should be(4)
    }
  }

}
