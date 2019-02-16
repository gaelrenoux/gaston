package fr.renoux.gaston.engine

import fr.renoux.gaston.{SimpleTestModel, UdoConTestModel}
import org.scalatest.{FlatSpec, Matchers}

class ScorerSpec extends FlatSpec with Matchers {

  "score" should "give the correct result for a simple problem" in {
    Scorer.score(SimpleTestModel.Solutions.Best).value should be(10.2265625)
  }

  it should "give the correct result for a real-life problem" in {
    Scorer.score(UdoConTestModel.Solutions.Actual).value should be(6.510881905754407)
  }
}
