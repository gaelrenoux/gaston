package fr.renoux.gaston.engine

import fr.renoux.gaston.SimpleTestModel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ScoreCalculatorSpec extends AnyFlatSpec with Matchers {
  "score" should "give the correct result for a simple problem" in {
    SimpleTestModel.Solutions.Best.score.value should be(10.2265625)
  }
}
