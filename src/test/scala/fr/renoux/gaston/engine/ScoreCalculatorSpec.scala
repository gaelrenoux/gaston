package fr.renoux.gaston.engine

import fr.renoux.gaston.SimpleTestModel
import fr.renoux.gaston.model.Score
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ScoreCalculatorSpec extends AnyFlatSpec with Matchers {


  "score" should "give the correct result for a simple problem" in {
    SimpleTestModel.Solutions.BestWithUnassignedTopics.score.value should be(10.2265625)
  }

  it should "give a correct unweightedScoresByPerson" in {
    SimpleTestModel.Solutions.BestWithUnassignedTopics.scoreCalculator.unweightedScoresByPerson should be(Map(
      SimpleTestModel.Persons.Arthur -> Score(5.0),
      SimpleTestModel.Persons.Bianca -> Score(6.0),
      SimpleTestModel.Persons.Corwin -> Score(6.0),
      SimpleTestModel.Persons.Daniela -> Score(6.0),
      SimpleTestModel.Persons.Eric -> Score(5.0),
      SimpleTestModel.Persons.Fiona -> Score(6.0),
      SimpleTestModel.Persons.Garion -> Score(6.0),
      SimpleTestModel.Persons.Hercule -> Score(6.0),
      SimpleTestModel.Persons.Iago -> Score(5.0),
    ))
  }

  it should "give a correct weightedScoresByPerson" in {
    SimpleTestModel.Solutions.BestWithUnassignedTopics.scoreCalculator.weightedScoresByPerson should be(Map(
      SimpleTestModel.Persons.Arthur -> Score(5.0),
      SimpleTestModel.Persons.Bianca -> Score(6.0),
      SimpleTestModel.Persons.Corwin -> Score(6.0),
      SimpleTestModel.Persons.Daniela -> Score(6.0),
      SimpleTestModel.Persons.Eric -> Score(5.0),
      SimpleTestModel.Persons.Fiona -> Score(6.0),
      SimpleTestModel.Persons.Garion -> Score(6.0),
      SimpleTestModel.Persons.Hercule -> Score(6.0),
      SimpleTestModel.Persons.Iago -> Score(5.0),
    ))
  }
}
