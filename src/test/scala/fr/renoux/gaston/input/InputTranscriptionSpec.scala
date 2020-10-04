package fr.renoux.gaston.input

import eu.timepit.refined.auto._
import fr.renoux.gaston.model.constraints.TopicsSimultaneous
import fr.renoux.gaston.model.preferences.{PersonTopicPreference, TopicsExclusive}
import fr.renoux.gaston.model.{Problem, Score}
import fr.renoux.gaston.util.CollectionImplicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InputTranscriptionSpec extends AnyFlatSpec with Matchers {

  import fr.renoux.gaston.TestUtils._

  private def from(model: InputModel): Problem = {
    new InputTranscription(model).result.valueOr(e => throw new IllegalArgumentException(e.toString))
  }

  behavior of "Occurrences"

  {
    val inputTopics = List(InputTopic(
      name = "alpha",
      occurrences = Some(3)
    ))

    val topicNames = Set("alpha #1", "alpha #2", "alpha #3")

    def topics(implicit problem: Problem) = problem.topics.filter(t => topicNames.contains(t.name))

    they should "be different topics" in {
      val problem = from(InputModel(topics = inputTopics))
      problem.topics.map(_.name) should be(topicNames)
    }

    they should "all have the same mandatory, forbidden and wishes" in {
      implicit val problem = from(InputModel(
        topics = inputTopics,
        persons = List(
          InputPerson("Arnold", mandatory = Set("alpha")),
          InputPerson("Bianca", forbidden = Set("alpha")),
          InputPerson("Caroline", wishes = Map("alpha" -> Score(50))),
        )
      ))
      problem.mandatoryTopicsByPerson(p"Arnold") should be(topics)
      problem.forbiddenTopicsByPerson(p"Bianca") should be(topics)
      problem.preferencesByPerson.mapKeys(_.name)("Caroline").collect {
        case PersonTopicPreference(_, t, s) => (t, s)
      } should be(topics.map(_ -> Score.PersonTotalScore))
    }

    they should "be exclusive except for mandatory people" in {
      implicit val problem = from(InputModel(
        topics = inputTopics,
        persons = List(
          InputPerson("Arnold", mandatory = Set("alpha"))
        )
      ))
      problem.preferences.collect {
        case TopicsExclusive(ts, ex, _) => (ts, ex)
      } should be(Set(topics -> Set(p"Arnold")))
    }

  }



  behavior of "Multiple"

  {
    val inputTopics = List(InputTopic(
      name = "alpha",
      multiple = Some(3)
    ))

    val topicNames = Set("alpha ~1", "alpha ~2", "alpha ~3")

    def topics(implicit problem: Problem) = problem.topics.filter(t => topicNames.contains(t.name))

    they should "be transcribed to different topics" in {
      val problem = from(InputModel(topics = inputTopics))
      problem.topics.map(_.name) should be(topicNames)
    }

    they should "all have the same forbidden and wishes" in {
      implicit val problem = from(InputModel(
        topics = inputTopics,
        persons = List(
          InputPerson("Bianca", forbidden = Set("alpha")),
          InputPerson("Caroline", wishes = Map("alpha" -> Score(50))),
        )
      ))
      problem.forbiddenTopicsByPerson(p"Bianca") should be(topics)
      problem.preferencesByPerson(p"Caroline").collect {
        case PersonTopicPreference(_, t, s) => (t, s)
      } should be(topics.map(_ -> Score.PersonTotalScore))
    }

    they should "be simultaneous" in {
      implicit val problem = from(InputModel(
        topics = inputTopics,
        persons = List(
          InputPerson("Arnold", mandatory = Set("alpha"))
        )
      ))
      problem.constraints.collect {
        case TopicsSimultaneous(ts) => ts
      } should be(Set(topics))
    }

    they should "dispatch mandatory persons" in {
      implicit val problem = from(InputModel(
        topics = inputTopics,
        persons = List(
          InputPerson("Arnold", mandatory = Set("alpha")),
          InputPerson("Bianca", mandatory = Set("alpha"))
        )
      ))

      problem.mandatoryTopicsByPerson(p"Arnold") should be(Set(t"alpha ~1"))
      problem.mandatoryTopicsByPerson(p"Bianca") should be(Set(t"alpha ~2"))
    }

  }

}
