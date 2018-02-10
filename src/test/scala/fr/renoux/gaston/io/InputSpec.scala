package fr.renoux.gaston.io

import fr.renoux.gaston.UdoConTestModel
import fr.renoux.gaston.model.Score
import org.scalatest.{FlatSpec, Matchers}

class InputSpec extends FlatSpec with Matchers {

  behavior of "fromClassPath"
  it should "load the input from the ClassPath" in {
    val (input, problem) = InputLoader.fromClassPath.forceToInputAndModel
    input.gaston.settings.weakPreference should be (Score(1))
    problem.persons.size should be >0
  }

  "conversion" should "work" in {
    val problem = InputLoader.fromClassPath.forceToModel
    problem.slots should be (UdoConTestModel.Problems.Simplified.slots)
    problem.topics should be (UdoConTestModel.Problems.Simplified.topics)
    problem.persons should be (UdoConTestModel.Problems.Simplified.persons)
    problem.constraints should be (UdoConTestModel.Problems.Simplified.constraints)
    problem.preferences should be (UdoConTestModel.Problems.Simplified.preferences)
    problem should be (UdoConTestModel.Problems.Simplified)
  }

}
