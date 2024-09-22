package fr.renoux.gaston.engine

import fr.renoux.gaston.SimpleTestModel
import fr.renoux.gaston.model.FlatScore
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ScoreCalculatorSpec extends AnyFlatSpec with Matchers {


  "score" should "give the correct result for a simple problem" in {
    SimpleTestModel.Solutions.Best.score.value should be(10.2265625)
  }

  it should "give a correct unweightedScoresByPerson" in {
    SimpleTestModel.Solutions.Best.scoreCalculator.unweightedScoresByPerson should be(Map(
      SimpleTestModel.Persons.Arthur -> FlatScore(5.0),
      SimpleTestModel.Persons.Bianca -> FlatScore(6.0),
      SimpleTestModel.Persons.Corwin -> FlatScore(6.0),
      SimpleTestModel.Persons.Daniela -> FlatScore(6.0),
      SimpleTestModel.Persons.Eric -> FlatScore(5.0),
      SimpleTestModel.Persons.Fiona -> FlatScore(6.0),
      SimpleTestModel.Persons.Garion -> FlatScore(6.0),
      SimpleTestModel.Persons.Hercule -> FlatScore(6.0),
      SimpleTestModel.Persons.Iago -> FlatScore(5.0),
    ))
  }

  it should "give a correct weightedScoresByPerson" in {
    SimpleTestModel.Solutions.Best.scoreCalculator.weightedScoresByPerson should be(Map(
      SimpleTestModel.Persons.Arthur -> FlatScore(5.0),
      SimpleTestModel.Persons.Bianca -> FlatScore(6.0),
      SimpleTestModel.Persons.Corwin -> FlatScore(6.0),
      SimpleTestModel.Persons.Daniela -> FlatScore(6.0),
      SimpleTestModel.Persons.Eric -> FlatScore(5.0),
      SimpleTestModel.Persons.Fiona -> FlatScore(6.0),
      SimpleTestModel.Persons.Garion -> FlatScore(6.0),
      SimpleTestModel.Persons.Hercule -> FlatScore(6.0),
      SimpleTestModel.Persons.Iago -> FlatScore(5.0),
    ))
  }
}
