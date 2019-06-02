package fr.renoux.gaston.input

import fr.renoux.gaston.model.preferences.PersonTopicPreference
import fr.renoux.gaston.model.{Problem, Score}
import fr.renoux.gaston.util.CollectionImplicits._
import org.scalatest.{FlatSpec, Matchers}

class InputTranscriptionSpec extends FlatSpec with Matchers {

  private def from(model: InputModel): Problem = {
    new InputTranscription(InputRoot(model)).problem.valueOr(e => throw new IllegalArgumentException(e.toString))
  }

  behavior of "Occurrences"

  {
    val inputTopics = Set(InputTopic(
      name = "alpha",
      occurrences = Some(3)
    ))

    val topicNames = Set("alpha #1", "alpha #2", "alpha #3")

    they should "be transcribed to different topics" in {
      val problem = from(InputModel(topics = inputTopics))
      problem.topics.map(_.name) should be(topicNames)
    }

    they should "all have the same wishes" in {
      val problem = from(InputModel(
        topics = inputTopics,
        persons = Set(
          InputPerson("Arnold", mandatory = Set("alpha")),
          InputPerson("Bianca", forbidden = Set("alpha")),
          InputPerson("Caroline", wishes = Map("alpha" -> Score(50))),
        )
      ))
      problem.mandatoryTopicsPerPerson.mapKeys(_.name)("Arnold").map(_.name) should be(topicNames)
      problem.forbiddenTopicsPerPerson.mapKeys(_.name)("Bianca").map(_.name) should be(topicNames)
      problem.preferencesPerPerson.mapKeys(_.name)("Caroline").collect {
        case PersonTopicPreference(_, t, s) => (t.name, s)
      } should be(topicNames.map(_ -> Score.PersonTotalScore))
    }

  }

}
