package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase


class ScoreWeightTest extends TestBase {

  "Score*Weight" in {
    val s: Score = 100
    val w: Weight = 0.5
    (s <*> w) should be(50)
    (w <*> s) should be(50)
  }
}
