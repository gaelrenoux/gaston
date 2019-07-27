package fr.renoux.gaston.engine

import fr.renoux.gaston.SimpleTestModel
import fr.renoux.gaston.model.Scorer
import org.scalatest.{FlatSpec, Matchers}

class ScorerSpec extends FlatSpec with Matchers {
  private implicit val context: Context = Context.Default

  "score" should "give the correct result for a simple problem" in {
    Scorer.score(SimpleTestModel.Solutions.Best).value should be(10.2265625)
  }
}
