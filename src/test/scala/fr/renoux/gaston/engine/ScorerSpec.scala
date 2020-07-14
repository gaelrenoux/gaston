package fr.renoux.gaston.engine

import fr.renoux.gaston.SimpleTestModel
import fr.renoux.gaston.model.Scorer
import fr.renoux.gaston.util.Context
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ScorerSpec extends AnyFlatSpec with Matchers {
  private implicit val context: Context = Context.Default

  "score" should "give the correct result for a simple problem" in {
    Scorer.score(SimpleTestModel.Solutions.Best).value should be(10.2265625)
  }
}
