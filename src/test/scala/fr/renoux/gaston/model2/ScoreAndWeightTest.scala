package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase

import scala.util.Random


class ScoreAndWeightTest extends TestBase {

  val rand = new Random(0)

  "Score" - {
    "+" in {
      (2: Score) + (3: Score) should be(5: Score)
    }

    "*" in {
      (2: Score) * 3 should be(6: Score)
      (2: Score) * (3: Weight) should be(6: Score)
      (2: Score) * (3: Count[TopicId]) should be(6: Score)
      (2: Score) * CountAll[TopicId](3) should be(6: Score)
    }

    ">" in {
      (2: Score) > (3: Score) should be(false)
      (3: Score) > (3: Score) should be(false)
      (5: Score) > (3: Score) should be(true)
    }

    "<" in {
      (2: Score) < (3: Score) should be(true)
      (3: Score) < (3: Score) should be(false)
      (5: Score) < (3: Score) should be(false)
    }

    ">=" in {
      (2: Score) >= (3: Score) should be(false)
      (3: Score) >= (3: Score) should be(true)
      (5: Score) >= (3: Score) should be(true)
    }

    "<=" in {
      (2: Score) <= (3: Score) should be(true)
      (3: Score) <= (3: Score) should be(true)
      (5: Score) <= (3: Score) should be(false)
    }

    "sort" in {
      val scores = Array.fill(100)(rand.nextDouble(): Score)
      Score.sort(scores)
      scores.map(_.value) shouldBe sorted
    }

    "ordering" in {
      val scores = List.fill(100)(rand.nextDouble(): Score)
      scores.sorted.map(_.value) shouldBe sorted
    }

    "toPrettyString" in {
      (3.14: Score).toPrettyString should be("3.14")
    }

  }


  "Weight" - {
    "*" in {
      (3: Weight) * (2: Score) should be(6: Score)
    }

    "toPrettyString" in {
      (3.14: Weight).toPrettyString should be("3.14")
    }
  }
}
