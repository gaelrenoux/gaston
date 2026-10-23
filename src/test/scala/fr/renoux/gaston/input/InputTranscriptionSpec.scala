package fr.renoux.gaston.input

import com.softwaremill.quicklens.*
import fr.renoux.gaston.model.constraints.{TopicsLimitedCount, TopicsNotSimultaneous, TopicsSimultaneous}
import fr.renoux.gaston.model.preferences.*
import fr.renoux.gaston.model.Counts.given
import fr.renoux.gaston.model.{Counts, Problem, Score, Weight, toArraySet}
import io.github.iltotore.iron.autoRefine
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InputTranscriptionSpec extends AnyFlatSpec with Matchers {
  // TODO Improve this test !
  // TODO missing test for Simultaneous
  // TODO missing test for NotSimultaneous
  // TODO missing test for adjusted absence ratio

  import fr.renoux.gaston.TestUtils.*

  private def from(model: InputModel): Problem = {
    new InputTranscription(model).result.valueOr(e => throw new IllegalArgumentException(e.toString))
  }

  behavior of "Occurrences"

  {
    val inputSlots = List(List(InputSlot("afternoon"), InputSlot("evening")))
    val inputTopics = List(InputTopic(
      name = "alpha",
      occurrences = Some(3)
    ))

    val topicNames = Set("alpha #1", "alpha #2", "alpha #3")

    def topics(using problem: Problem) = problem.topicsSet.filter(t => topicNames.contains(t.name))

    they should "be different topics" in {
      val problem = from(InputModel(topics = inputTopics))
      problem.topicsSet.map(_.name) should be(topicNames)
    }

    they should "all have the same mandatory, forbidden and wishes" in {
      given problem: Problem = from(InputModel(
        slots = inputSlots,
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
      given problem: Problem = from(InputModel(
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
      given problem: Problem = from(inputModel)

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
      given problem: Problem = from(
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

      given problem: Problem = from(inputModel)

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

      given problem: Problem = from(inputModel)

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

      given problem: Problem = from(inputModel)

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

      given problem: Problem = from(inputModel)

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

  behavior of "Constraints"

  {
    val inputSlots = List(List(InputSlot("one"), InputSlot("two")))
    val inputTopics = List(InputTopic("alpha"), InputTopic("beta"), InputTopic("gamma"))

    it should "create simultaneous constraints" in {
      given problem: Problem = from(InputModel(
        slots = inputSlots,
        topics = inputTopics,
        constraints = InputGlobalConstraints(simultaneous = Set(InputSimultaneousConstraint(Set("alpha", "beta"))))
      ))
      problem.constraints should be(Set(TopicsSimultaneous(Set(t"alpha", t"beta"))))
    }

    it should "match occurrences one-to-one in simultaneous constraints" in {
      given problem: Problem = from(InputModel(
        slots = inputSlots,
        topics = List(InputTopic("alpha", occurrences = Some(2)), InputTopic("beta", occurrences = Some(2))),
        constraints = InputGlobalConstraints(simultaneous = Set(InputSimultaneousConstraint(Set("alpha", "beta"))))
      ))
      problem.constraints should be(Set(
        TopicsSimultaneous(Set(t"alpha #1", t"beta #1")),
        TopicsSimultaneous(Set(t"alpha #2", t"beta #2"))
      ))
    }

    it should "create not-simultaneous constraints" in {
      given problem: Problem = from(InputModel(
        slots = inputSlots,
        topics = inputTopics,
        constraints = InputGlobalConstraints(notSimultaneous = Set(InputSimultaneousConstraint(Set("alpha", "beta", "gamma"))))
      ))
      given Counts = problem.counts
      problem.constraints should be(Set(TopicsNotSimultaneous(Set(t"alpha", t"beta", t"gamma").toArraySet)))
    }

    it should "create limited-count constraints" in {
      given problem: Problem = from(InputModel(
        slots = inputSlots,
        topics = inputTopics,
        constraints = InputGlobalConstraints(limitedCount = Set(InputLimitedTopicsCountConstraint(Set("alpha", "beta", "gamma"), 2)))
      ))
      problem.constraints.toList match {
        case List(TopicsLimitedCount(topics, count)) =>
          topics.toIdSet should be(Set(t"alpha", t"beta", t"gamma").map(_.id))
          count should be(2)
        case other => fail(s"Unexpected constraints: $other")
      }
    }

    it should "create exclusive preferences, with exemptions or inclusions" in {
      given problem: Problem = from(InputModel(
        slots = inputSlots,
        topics = inputTopics,
        persons = List(InputPerson("Arnold"), InputPerson("Bianca")),
        constraints = InputGlobalConstraints(exclusive = Set(
          InputExclusiveConstraint(Set("alpha", "beta"), exemptions = Some(Set("Arnold"))),
          InputExclusiveConstraint(Set("beta", "gamma"), inclusions = Some(Set("Bianca")))
        ))
      ))
      given Counts = problem.counts
      problem.preferences should be(Set(
        TopicsExclusive(Set(t"alpha", t"beta").toArraySet, Set(p"Arnold").toArraySet),
        TopicsExclusiveFor(Set(t"beta", t"gamma").toArraySet, Set(p"Bianca").toArraySet)
      ))
    }

    it should "create linked preferences" in {
      given problem: Problem = from(InputModel(
        slots = inputSlots,
        topics = inputTopics,
        constraints = InputGlobalConstraints(linked = Set(InputLinkedConstraint(Set("alpha", "beta"))))
      ))
      given Counts = problem.counts
      problem.preferences should be(Set(TopicsLinked(Set(t"alpha", t"beta").toArraySet)))
    }
  }


  behavior of "Topics"

  {
    val inputSlots = List(List(InputSlot("one"), InputSlot("two")), List(InputSlot("three")))

    it should "create linked parts with a followup for long topics" in {
      given problem: Problem = from(InputModel(
        slots = inputSlots,
        topics = List(InputTopic("alpha", duration = Some(2)))
      ))
      given Counts = problem.counts
      problem.topicsSet.map(_.name) should be(Set("alpha ~1", "alpha ~2"))
      t"alpha ~1".followup should be(Some(t"alpha ~2"))
      t"alpha ~1".isFollowup should be(false)
      t"alpha ~2".followup should be(None)
      t"alpha ~2".isFollowup should be(true)
      problem.preferences should be(Set(TopicsLinked(Set(t"alpha ~1", t"alpha ~2").toArraySet)))
    }

    it should "use the default min and max persons, unless overridden" in {
      given problem: Problem = from(InputModel(
        settings = InputSettings(defaultMinPersonsPerTopic = 3, defaultMaxPersonsPerTopic = 7),
        slots = inputSlots,
        topics = List(InputTopic("alpha"), InputTopic("beta", min = Some(2), max = Some(4)))
      ))
      (t"alpha".min, t"alpha".max) should be((3, 7))
      (t"beta".min, t"beta".max) should be((2, 4))
    }

    it should "handle forced topics, slot restrictions and presence scores" in {
      given problem: Problem = from(InputModel(
        slots = inputSlots,
        topics = List(InputTopic("alpha", forced = true, slots = Some(Set("one", "three")), presence = Some(Score(42))))
      ))
      t"alpha".forced should be(true)
      t"alpha".slots should be(Some(Set(slot"one", slot"three")))
      problem.preferences should be(Set(TopicDirectPreference(t"alpha", Score(42))))
    }

    it should "apply the default max topics per slot, unless overridden" in {
      given problem: Problem = from(InputModel(
        settings = InputSettings(defaultMaxTopicsPerSlot = Some(4)),
        slots = List(List(InputSlot("one"), InputSlot("two", maxTopics = Some(2)))),
      ))
      slot"one".maxTopics should be(4)
      slot"two".maxTopics should be(2)
    }

    it should "chain slots only inside a sequence" in {
      given problem: Problem = from(InputModel(slots = inputSlots))
      slot"one".next should be(Some(slot"two"))
      slot"two".next should be(None)
      slot"three".next should be(None)
    }
  }


  behavior of "Persons"

  {
    val inputSlots = List(List(InputSlot("one"), InputSlot("two")))
    val inputTopics = List(InputTopic("alpha"), InputTopic("beta"))

    it should "not have absent persons present on a slot" in {
      given problem: Problem = from(InputModel(
        slots = inputSlots,
        topics = inputTopics,
        persons = List(InputPerson("Arnold", absences = Set("one")), InputPerson("Bianca"))
      ))
      slot"one".personsPresent should be(Set(p"Bianca"))
      slot"two".personsPresent should be(Set(p"Arnold", p"Bianca"))
    }

    it should "scale up the wishes of absent persons" in {
      given problem: Problem = from(InputModel(
        slots = inputSlots,
        topics = inputTopics,
        persons = List(InputPerson("Arnold", absences = Set("one"), wishes = Map("alpha" -> Score(50))))
      ))
      /* Absent on half the slots, with the default adjustment factor of 0.8 */
      val expectedScore = Constants.PersonTotalScore.value / (1 - 0.5 * 0.8)
      problem.preferences.toList match {
        case List(PersonTopicPreference(_, _, score)) => score.value should be(expectedScore +- 0.001)
        case other => fail(s"Unexpected preferences: $other")
      }
    }

    it should "create anti-preferences for incompatible persons" in {
      given problem: Problem = from(InputModel(
        settings = InputSettings(incompatibilityAntiPreference = NonPosScore(-42.0)),
        slots = inputSlots,
        topics = inputTopics,
        persons = List(InputPerson("Arnold", incompatible = Set("Bianca", "Caroline")), InputPerson("Bianca"), InputPerson("Caroline"))
      ))
      given Counts = problem.counts
      problem.preferences should be(Set(PersonGroupAntiPreference(p"Arnold", Set(p"Bianca", p"Caroline").toArraySet, Score(-42))))
    }

    it should "keep the weight and base score" in {
      given problem: Problem = from(InputModel(
        slots = inputSlots,
        topics = inputTopics,
        persons = List(InputPerson("Arnold", weight = Weight(2), baseScore = Score(12)))
      ))
      p"Arnold".weight should be(Weight(2))
      p"Arnold".baseScore should be(Score(12))
    }

    it should "create free-slot preferences, and compensate them in the base score" in {
      given problem: Problem = from(InputModel(
        slots = inputSlots,
        topics = inputTopics,
        persons = List(InputPerson("Arnold", minFreeSlots = Some(1)))
      ))
      problem.preferences should be(Set(PersonFreeSlotPreference(p"Arnold", 1)))
      p"Arnold".baseScore should be(Score(1000))
    }
  }


  behavior of "Errors"

  {
    val inputSlots = List(List(InputSlot("one"), InputSlot("two")))
    val inputTopics = List(InputTopic("alpha"), InputTopic("beta"))

    def errorsOf(model: InputModel): List[String] =
      InputTranscription.transcribe(model).left.toOption.fold(Nil)(_.toList.map(_.description))

    it should "report duplicate names" in {
      errorsOf(InputModel(
        slots = List(List(InputSlot("one"), InputSlot("one"))),
        topics = List(InputTopic("alpha"), InputTopic("alpha")),
        persons = List(InputPerson("Arnold"), InputPerson("Arnold"))
      )) should be(List("Duplicate person name: Arnold", "Duplicate slot name: one", "Duplicate topic name: alpha"))
    }

    it should "report incorrect topics" in {
      errorsOf(InputModel(
        slots = inputSlots,
        topics = List(
          InputTopic("@alpha"),
          InputTopic("beta", min = Some(5), max = Some(4)),
          InputTopic("gamma~"),
          InputTopic("delta", slots = Some(Set("three")))
        )
      )).size should be(4)
    }

    it should "report persons referencing unknown entities" in {
      errorsOf(InputModel(
        slots = inputSlots,
        topics = inputTopics,
        persons = List(InputPerson(
          "Arnold",
          absences = Set("three"),
          mandatory = Set("gamma"),
          forbidden = Set("delta"),
          incompatible = Set("Zorro"),
          wishes = Map("epsilon" -> Score(1))
        ))
      )).size should be(5)
    }

    it should "report constraints referencing unknown topics" in {
      errorsOf(InputModel(
        slots = inputSlots,
        topics = inputTopics,
        constraints = InputGlobalConstraints(
          simultaneous = Set(InputSimultaneousConstraint(Set("alpha", "gamma"))),
          notSimultaneous = Set(InputSimultaneousConstraint(Set("alpha", "delta"))),
          exclusive = Set(InputExclusiveConstraint(Set("alpha", "epsilon"))),
          linked = Set(InputLinkedConstraint(Set("alpha", "zeta")))
        )
      )).size should be(4)
    }

    it should "report simultaneous topics with different occurrence counts" in {
      errorsOf(InputModel(
        slots = inputSlots,
        topics = List(InputTopic("alpha", occurrences = Some(2)), InputTopic("beta")),
        constraints = InputGlobalConstraints(simultaneous = Set(InputSimultaneousConstraint(Set("alpha", "beta"))))
      )).size should be(1)
    }

    it should "report persons wishing for unknown persons" in {
      errorsOf(InputModel(
        slots = inputSlots,
        topics = inputTopics,
        persons = List(InputPerson("Arnold", personWishes = Map("Zorro" -> Score(1))))
      )).size should be(1)
    }

    it should "report limited-count constraints with unknown topics" in {
      errorsOf(InputModel(
        slots = inputSlots,
        topics = List(InputTopic("alpha"), InputTopic("beta"), InputTopic("gamma")),
        constraints = InputGlobalConstraints(limitedCount = Set(InputLimitedTopicsCountConstraint(Set("alpha", "beta", "zeta"), 2)))
      )).size should be(1)
    }

    it should "report limited-count constraints whose count is not lower than their number of topics" in {
      errorsOf(InputModel(
        slots = inputSlots,
        topics = inputTopics,
        constraints = InputGlobalConstraints(limitedCount = Set(InputLimitedTopicsCountConstraint(Set("alpha", "beta"), 2)))
      )).size should be(1)
    }

    it should "count each occurrence and part of a topic in limited-count constraints" in {
      errorsOf(InputModel(
        slots = inputSlots,
        topics = List(InputTopic("alpha", occurrences = Some(2)), InputTopic("beta", duration = Some(2))),
        constraints = InputGlobalConstraints(limitedCount = Set(InputLimitedTopicsCountConstraint(Set("alpha", "beta"), 3)))
      )) should be(Nil)
    }
  }
}
