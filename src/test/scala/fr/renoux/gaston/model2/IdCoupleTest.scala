package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase


class IdCoupleTest extends TestBase {
  
  val i: SlotId = 13
  val j: TopicId = 42

  "nominal" in {
    val couple = IdCouple(i, j)
    couple._1 should be(i)
    couple._2 should be(j)
  }
  
  "zero" - {
    "on the left" in {
      val couple = IdCouple(0, j)
      couple._1 should be(0)
      couple._2 should be(j)
    }
    "on the right" in {
      val couple = IdCouple(i, 0)
      couple._1 should be(i)
      couple._2 should be(0)
    }
    "on both sides" in {
      val couple = IdCouple(0, 0)
      couple._1 should be(0)
      couple._2 should be(0)
    }
  }
  
  "-1" - {
    "on the left" in {
      val couple = IdCouple(-1, j)
      couple._1 should be(-1)
      couple._2 should be(j)
    }
    "on the right" in {
      val couple = IdCouple(i, -1)
      couple._1 should be(i)
      couple._2 should be(-1)
    }
    "on both sides" in {
      val couple = IdCouple(-1, -1)
      couple._1 should be(-1)
      couple._2 should be(-1)
    }
  }

  "MinValue" - {
    "on the left" in {
      val couple = IdCouple(Int.MinValue, j)
      couple._1 should be(Int.MinValue)
      couple._2 should be(j)
    }
    "on the right" in {
      val couple = IdCouple(i, Int.MinValue)
      couple._1 should be(i)
      couple._2 should be(Int.MinValue)
    }
    "on both sides" in {
      val couple = IdCouple(Int.MinValue, Int.MinValue)
      couple._1 should be(Int.MinValue)
      couple._2 should be(Int.MinValue)
    }
  }

  "MaxValue" - {
    "on the left" in {
      val couple = IdCouple(Int.MaxValue, j)
      couple._1 should be(Int.MaxValue)
      couple._2 should be(j)
    }
    "on the right" in {
      val couple = IdCouple(i, Int.MaxValue)
      couple._1 should be(i)
      couple._2 should be(Int.MaxValue)
    }
    "on both sides" in {
      val couple = IdCouple(Int.MaxValue, Int.MaxValue)
      couple._1 should be(Int.MaxValue)
      couple._2 should be(Int.MaxValue)
    }
  }
}
