package fr.renoux.gaston.model

import fr.renoux.gaston.SimpleTestModel
import fr.renoux.gaston.model.*
import fr.renoux.gaston.model.preferences.{PersonTopicPreference, TopicDirectPreference}
import fr.renoux.gaston.util.{ArrayMap, Context}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class ScoreCalculatorSpec extends AnyFlatSpec with Matchers {

  import ScoreCalculatorSpec.{*, given}

  "score" should "give the correct result for a simple problem" in {
    SimpleTestModel.Solutions.BestWithUnassignedTopics.score.value should be(13.1 +- 0.1)
  }

  it should "give a correct unweightedScoresByPerson" in {
    SimpleTestModel.Solutions.BestWithUnassignedTopics.scoreCalculator.unweightedScoresByPerson should be(Map(
      SimpleTestModel.Persons.Arthur -> Score(5.0),
      SimpleTestModel.Persons.Bianca -> Score(6.0),
      SimpleTestModel.Persons.Corwin -> Score(7.0), // he has a base score of 1
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
      SimpleTestModel.Persons.Bianca -> Score(3.0), // she has a weight of 2
      SimpleTestModel.Persons.Corwin -> Score(7.0), // he has a base score of 1
      SimpleTestModel.Persons.Daniela -> Score(6.0),
      SimpleTestModel.Persons.Eric -> Score(5.0),
      SimpleTestModel.Persons.Fiona -> Score(6.0),
      SimpleTestModel.Persons.Garion -> Score(6.0),
      SimpleTestModel.Persons.Hercule -> Score(6.0),
      SimpleTestModel.Persons.Iago -> Score(5.0),
    ))
  }

  it should "give a correct impersonalScore" in {
    SimpleTestModel.Solutions.BestWithUnassignedTopics.scoreCalculator.impersonalScore should be(5.0)
  }

  "Weights" should "divide the personal scores" in {
    given Problem = weightedProblem()
    val schedule = Schedule.from(Morning(Chess(Heavy), Go(Light)))
    schedule.scoreCalculator.unweightedScoresByPerson should be(Map(Heavy -> Score(10), Light -> Score(4)))
    schedule.scoreCalculator.weightedScoresByPerson should be(Map(Heavy -> Score(5), Light -> Score(4)))
  }

  it should "change which person is the least satisfied" in {
    given Problem = weightedProblem()
    /* Sorted weighted scores are 4 (Light) then 5 (Heavy): 4 + 5 / 2 */
    Schedule.from(Morning(Chess(Heavy), Go(Light))).score should be(Score(6.5))
    /* Sorted weighted scores are 0 (Heavy) then 4 (Light): 0 + 4 / 2 */
    Schedule.from(Morning(Go(Heavy, Light))).score should be(Score(2))
  }

  "Base scores" should "be added to the personal score" in {
    val lightWithBaseScore = Light.copy(baseScore = Score(3))
    given Problem = weightedProblem(light = lightWithBaseScore)
    val schedule = Schedule.from(Morning(Chess(Heavy), Go(lightWithBaseScore)))
    schedule.scoreCalculator.unweightedScoresByPerson should be(Map(Heavy -> Score(10), lightWithBaseScore -> Score(7)))
    /* Sorted weighted scores are 5 (Heavy) then 7 (Light): 5 + 7 / 2 */
    schedule.score should be(Score(8.5))
  }

  "Impersonal preferences" should "be added to the personal score" in {
    given Problem = weightedProblem(goPresence = Score(100))
    Schedule.from(Morning(Chess(Heavy), Go(Light))).score should be(Score(106.5))
  }

  it should "give a negative-infinity score when one of them is negative infinity" in {
    given Problem = weightedProblem(goPresence = Score.NegativeInfinity)
    Schedule.from(Morning(Chess(Heavy), Go(Light))).score.isNegativeInfinity should be(true)
  }

}

object ScoreCalculatorSpec {

  given Context = Context.Default

  val Heavy: Person = Person(0, "Heavy", weight = Weight(2))
  val Light: Person = Person(1, "Light")
  val Morning: Slot = Slot(0, "morning", Set(Heavy, Light), None)
  val Chess: Topic = Topic(0, "chess")
  val Go: Topic = Topic(1, "go")

  def weightedProblem(light: Person = Light, goPresence: Score = Score.Zero): Problem = {
    given Counts = Counts(slots = 1, topics = 2, persons = 2)
    val presence = if (goPresence.isZero) None else Some(TopicDirectPreference(Go, goPresence))
    new Problem(
      Seq(Seq(Morning)),
      Set(Chess, Go),
      ArrayMap.empty[Slot, Topic],
      Set(Heavy, light),
      Set.empty,
      Set[Preference](PersonTopicPreference(Heavy, Chess, Score(10)), PersonTopicPreference(light, Go, Score(4))) ++ presence
    )
  }
}
