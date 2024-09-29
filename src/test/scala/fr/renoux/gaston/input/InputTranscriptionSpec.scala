package fr.renoux.gaston.input

import com.softwaremill.quicklens._
import eu.timepit.refined.auto._
import fr.renoux.gaston.input.InputRefinements.NonPosScore
import fr.renoux.gaston.model.preferences.{PersonPersonPreference, PersonTopicPreference, TopicsExclusive}
import fr.renoux.gaston.model.{Problem, Score}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InputTranscriptionSpec extends AnyFlatSpec with Matchers {
  // TODO Improve this test !
  // TODO missing test for Simultaneous
  // TODO missing test for NotSimultaneous


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

    def topics(implicit problem: Problem) = problem.topicsSet.filter(t => topicNames.contains(t.name))

    they should "be different topics" in {
      val problem = from(InputModel(topics = inputTopics))
      problem.topicsSet.map(_.name) should be(topicNames)
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
      problem.preferencesByPerson(p"Caroline").collect {
        case PersonTopicPreference(_, t, s) => (t, s)
      } should be(topics.map(_ -> Constants.PersonTotalScore))
    }

    they should "be exclusive except for mandatory people" in {
      implicit val problem = from(InputModel(
        topics = inputTopics,
        persons = List(
          InputPerson("Arnold", mandatory = Set("alpha")),
          InputPerson("Willy", mandatory = Set())
        )
      ))
      val expected = Set(Array.fill(3)(true).toSeq -> Array(true, false).toSeq)
      problem.preferences.collect {
        case TopicsExclusive(ts, ex, _) => (ts.safeContent, ex.safeContent)
      } should be(expected)

    }

  }



  behavior of "Unassigned"

  {
    val inputSettings = InputSettings(unassigned = InputSettings.Unassigned(allowed = true, maxPersons = 3, personAntiPreference = NonPosScore(-42.0)))
    val inputSlots = List(List(InputSlot("one"), InputSlot("two")))
    val inputTopics = List(InputTopic("alpha"), InputTopic("beta"), InputTopic("gamma"))
    val inputPersons = List(
      InputPerson("Arnold", mandatory = Set("alpha")),
      InputPerson("Bianca", forbidden = Set("alpha")),
      InputPerson("Caroline", wishes = Map("alpha" -> Score(50)))
    )
    val inputModel = InputModel(
      settings = inputSettings,
      slots = inputSlots,
      topics = inputTopics,
      persons = inputPersons
    )

    it should "have a basic anti-preference (when no scaling)" in {
      implicit val problem: Problem = from(inputModel)
      val arnoldNothingPreferences = problem.preferencesByPerson(p"Arnold").collect {
        case PersonTopicPreference(_, t, s) if t.name.startsWith("@Unassigned") => s
      }
      val biancaNothingPreferences = problem.preferencesByPerson(p"Bianca").collect {
        case PersonTopicPreference(_, t, s) if t.name.startsWith("@Unassigned") => s
      }
      val carolineNothingPreferences = problem.preferencesByPerson(p"Caroline").collect {
        case PersonTopicPreference(_, t, s) if t.name.startsWith("@Unassigned") => s
      }
      arnoldNothingPreferences.size should be(inputSlots.size)
      arnoldNothingPreferences.foreach(_ should be(Score(-42)))
      biancaNothingPreferences.size should be(inputSlots.size)
      biancaNothingPreferences.foreach(_ should be(Score(-42)))
      carolineNothingPreferences.size should be(inputSlots.size)
      carolineNothingPreferences.foreach(_ should be(Score(-42)))
    }

    it should "have a scaled anti-preference (with scaling enabled)" in {
      implicit val problem: Problem = from(
        inputModel.modify(_.settings.unassigned.personAntiPreferenceScaling).setTo(Some(
          InputSettings.UnassignedAntiPreferenceScaling(
            forbiddenRatioForMaximum = 0.5,
            maximumAntiPreference = NonPosScore(-12.0)
          )
        ))
      )
      val arnoldNothingPreferences = problem.preferencesByPerson(p"Arnold").collect {
        case PersonTopicPreference(_, t, s) if t.name.startsWith("@Unassigned") => s
      }
      val biancaNothingPreferences = problem.preferencesByPerson(p"Bianca").collect {
        case PersonTopicPreference(_, t, s) if t.name.startsWith("@Unassigned") => s
      }
      val carolineNothingPreferences = problem.preferencesByPerson(p"Caroline").collect {
        case PersonTopicPreference(_, t, s) if t.name.startsWith("@Unassigned") => s
      }
      arnoldNothingPreferences.size should be(inputSlots.size)
      arnoldNothingPreferences.foreach(_ should be(Score(-42)))
      biancaNothingPreferences.size should be(inputSlots.size)
      biancaNothingPreferences.foreach(_ should be(Score(-22)))
      carolineNothingPreferences.size should be(inputSlots.size)
      carolineNothingPreferences.foreach(_ should be(Score(-42)))
    }
  }



  behavior of "Incompatible topics per slot"

  {
    it should "work for slot-specific topics" in {
      val inputSlots = List(List(InputSlot("one"), InputSlot("two")))
      val inputTopics = List(InputTopic("alpha", slots = Some(Set("two"))), InputTopic("beta"))
      val inputPersons = List(InputPerson("Arnold", mandatory = Set("alpha")))
      val inputModel = InputModel(
        slots = inputSlots,
        topics = inputTopics,
        persons = inputPersons
      )
      implicit val problem: Problem = from(inputModel)
      problem.incompatibleTopicsBySlot(slot"one") should be(Set(t"alpha"))
      problem.incompatibleTopicsBySlot(slot"two") should be(Set.empty)
    }

  }



  behavior of "Wishes"

  {
    it should "load topic wishes" in {
      val inputSlots = List(List(InputSlot("one"), InputSlot("two")))
      val inputTopics = List(InputTopic("alpha"), InputTopic("beta"), InputTopic("gamma"))
      val inputPersons = List(InputPerson("Arnold", wishes = Map("alpha" -> Score(50), "gamma" -> Score(100))))
      val inputModel = InputModel(
        slots = inputSlots,
        topics = inputTopics,
        persons = inputPersons
      )
      implicit val problem: Problem = from(inputModel)
      val arnold = problem.personsList.find(_.name == "Arnold").get
      val alpha = problem.topicsList.find(_.name == "alpha").get
      val gamma = problem.topicsList.find(_.name == "gamma").get
      val wishes = problem.personalPreferencesListByPerson(arnold)
      wishes.size should be(2)
      val wishAlpha = wishes.collectFirst { case ptp: PersonTopicPreference if ptp.topic.name == "alpha" => ptp }
      wishAlpha.nonEmpty should be(true)
      wishAlpha.get.person should be(arnold)
      wishAlpha.get.topic should be(alpha)
      wishAlpha.get.reward.value should be(333.3 +- 0.1)
      val wishGamma = wishes.collectFirst { case ptp: PersonTopicPreference if ptp.topic.name == "gamma" => ptp }
      wishGamma.nonEmpty should be(true)
      wishGamma.get.person should be(arnold)
      wishGamma.get.topic should be(gamma)
      wishGamma.get.reward.value should be(666.6 +- 0.1)
    }

    it should "load person wishes" in {
      val inputSlots = List(List(InputSlot("one"), InputSlot("two")))
      val inputTopics = List(InputTopic("alpha"), InputTopic("beta"), InputTopic("gamma"))
      val inputPersons = List(
        InputPerson("Arnold", personWishes = Map("Bianca" -> Score(50), "Charlie" -> Score(100))),
        InputPerson("Bianca"), InputPerson("Charlie")
      )
      val inputModel = InputModel(
        slots = inputSlots,
        topics = inputTopics,
        persons = inputPersons
      )
      implicit val problem: Problem = from(inputModel)
      val arnold = problem.personsList.find(_.name == "Arnold").get
      val bianca = problem.personsList.find(_.name == "Bianca").get
      val charlie = problem.personsList.find(_.name == "Charlie").get
      val wishes = problem.personalPreferencesListByPerson(arnold)
      wishes.size should be(2)
      val wishBianca = wishes.collectFirst { case ppp: PersonPersonPreference if ppp.targetPerson.name == "Bianca" => ppp }
      wishBianca.nonEmpty should be(true)
      wishBianca.get.person should be(arnold)
      wishBianca.get.targetPerson should be(bianca)
      wishBianca.get.reward.value should be(333.3 +- 0.1)
      val wishCharlie = wishes.collectFirst { case ppp: PersonPersonPreference if ppp.targetPerson.name == "Charlie" => ppp }
      wishCharlie.nonEmpty should be(true)
      wishCharlie.get.person should be(arnold)
      wishCharlie.get.targetPerson should be(charlie)
      wishCharlie.get.reward.value should be(666.6 +- 0.1)
    }

    it should "take topic and person wishes together" in {
      val inputSlots = List(List(InputSlot("one"), InputSlot("two")))
      val inputTopics = List(InputTopic("alpha"), InputTopic("beta"), InputTopic("gamma"))
      val inputPersons = List(
        InputPerson("Arnold", wishes = Map("alpha" -> Score(50)), personWishes = Map("Charlie" -> Score(100))),
        InputPerson("Bianca"), InputPerson("Charlie")
      )
      val inputModel = InputModel(
        slots = inputSlots,
        topics = inputTopics,
        persons = inputPersons
      )
      implicit val problem: Problem = from(inputModel)
      val arnold = problem.personsList.find(_.name == "Arnold").get
      val alpha = problem.topicsList.find(_.name == "alpha").get
      val charlie = problem.personsList.find(_.name == "Charlie").get
      val wishes = problem.personalPreferencesListByPerson(arnold)
      wishes.size should be(2)
      val wishAlpha = wishes.collectFirst { case ptp: PersonTopicPreference if ptp.topic.name == "alpha" => ptp }
      wishAlpha.nonEmpty should be(true)
      wishAlpha.get.person should be(arnold)
      wishAlpha.get.topic should be(alpha)
      wishAlpha.get.reward.value should be(333.3 +- 0.1)
      val wishCharlie = wishes.collectFirst { case ppp: PersonPersonPreference if ppp.targetPerson.name == "Charlie" => ppp }
      wishCharlie.nonEmpty should be(true)
      wishCharlie.get.person should be(arnold)
      wishCharlie.get.targetPerson should be(charlie)
      wishCharlie.get.reward.value should be(666.6 +- 0.1)
    }
  }
}
