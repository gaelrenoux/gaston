package fr.renoux.gaston.model

import fr.renoux.gaston.TestUtils.*
import fr.renoux.gaston.input.*
import fr.renoux.gaston.model.Counts.given
import fr.renoux.gaston.model.constraints.{TopicsNotSimultaneous, TopicsSimultaneous}
import fr.renoux.gaston.model.preferences.{PersonFreeSlotPreference, PersonTopicPreference, TopicDirectPreference, TopicsLinked}
import io.github.iltotore.iron.autoRefine
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProblemSpec extends AnyFlatSpec with Matchers {

  given problem: Problem = InputTranscription.transcribe(InputModel(
    slots = List(
      List(InputSlot("d1-am"), InputSlot("d1-pm")),
      List(InputSlot("d2-am"))
    ),
    topics = List(
      InputTopic("alpha"),
      InputTopic("beta"),
      InputTopic("gamma", slots = Some(Set("d2-am"))),
      InputTopic("delta", forced = true),
      InputTopic("epsilon", forced = true, slots = Some(Set("d1-am", "d2-am"))),
      InputTopic("zeta", forced = true, slots = Some(Set("d1-pm"))),
      InputTopic("long", duration = Some(2), presence = Some(Score(10)))
    ),
    persons = List(
      InputPerson("Arnold", mandatory = Set("alpha", "beta"), wishes = Map("gamma" -> Score(1))),
      InputPerson("Bianca", absences = Set("d1-pm"), mandatory = Set("delta"), forbidden = Set("alpha")),
      InputPerson("Caroline", baseScore = Score(12), minFreeSlots = Some(1))
    ),
    constraints = InputGlobalConstraints(
      simultaneous = Set(InputSimultaneousConstraint(Set("alpha", "gamma"))),
      notSimultaneous = Set(InputSimultaneousConstraint(Set("gamma", "delta")))
    )
  )).force

  given Counts = problem.counts

  behavior of "Slots indexes"

  it should "link each slot to the next one and the previous one, within a sequence only" in {
    problem.slotsToNextSlot should be(Map(slot"d1-am" -> slot"d1-pm"))
    problem.slotsToPreviousSlot should be(Map(slot"d1-pm" -> slot"d1-am"))
  }

  it should "list all slot couples once, with the first one before the second one by name" in {
    problem.slotCouplesList.toSet should be(Set(
      (slot"d1-am", slot"d1-pm"),
      (slot"d1-am", slot"d2-am"),
      (slot"d1-pm", slot"d2-am")
    ))
    problem.slotCouplesList.size should be(3)
  }

  behavior of "Topics indexes"

  it should "list forced topics, from the most constrained to the least constrained" in {
    problem.forcedTopics should be(Set(t"delta", t"epsilon", t"zeta"))
    problem.forcedTopicsMostToLeastConstrained should be(Seq(t"zeta", t"epsilon", t"delta"))
  }

  it should "list topics with their followup" in {
    problem.topicsWithFollowups should be(Set(t"long ~1" -> t"long ~2"))
  }

  it should "not have unassigned topics when they are not allowed" in {
    problem.hasUnassignedTopics should be(false)
  }

  behavior of "Persons indexes"

  it should "have the mandatory and forbidden topics of each person" in {
    problem.mandatoryTopicsByPerson(p"Arnold") should be(Set(t"alpha", t"beta"))
    problem.mandatoryTopicsByPerson(p"Bianca") should be(Set(t"delta"))
    problem.mandatoryTopicsByPerson(p"Caroline") should be(Set.empty)
    problem.forbiddenTopicsByPerson(p"Arnold") should be(Set.empty)
    problem.forbiddenTopicsByPerson(p"Bianca") should be(Set(t"alpha"))
  }

  it should "have a base score only for persons who have one" in {
    /* Caroline wants a free slot, so her base score is raised by the unassigned anti-preference to compensate */
    problem.baseScoreByPerson.keySet should be(problem.personsSet)
    problem.baseScoreByPerson(p"Arnold") should be(Score.Zero)
    problem.baseScoreByPerson(p"Caroline") should be(Score(12) + Score(1000))
  }

  behavior of "incompatibleTopicsByTopic"

  it should "make topics with a common mandatory person incompatible" in {
    problem.incompatibleTopicsByTopic(t"alpha") should contain(t"beta")
    problem.incompatibleTopicsByTopic(t"beta") should contain(t"alpha")
  }

  it should "make topics in a not-simultaneous constraint incompatible" in {
    problem.incompatibleTopicsByTopic(t"gamma") should contain(t"delta")
    problem.incompatibleTopicsByTopic(t"delta") should contain(t"gamma")
  }

  it should "not make unrelated topics incompatible" in {
    problem.incompatibleTopicsByTopic(t"alpha") should not contain t"gamma"
    problem.incompatibleTopicsByTopic(t"epsilon") should be(Set.empty)
  }

  behavior of "incompatibleTopicsBySlot"

  it should "exclude topics whose mandatory person is absent, or which are restricted to other slots" in {
    problem.incompatibleTopicsBySlot(slot"d1-am") should be(Set(t"gamma", t"zeta"))
    problem.incompatibleTopicsBySlot(slot"d1-pm") should be(Set(t"gamma", t"delta", t"epsilon"))
    problem.incompatibleTopicsBySlot(slot"d2-am") should be(Set(t"zeta"))
  }

  behavior of "simultaneousTopicByTopic"

  it should "list the other topics that must be simultaneous" in {
    problem.simultaneousTopicByTopic(t"alpha") should be(Set(t"gamma"))
    problem.simultaneousTopicByTopic(t"gamma") should be(Set(t"alpha"))
    problem.simultaneousTopicByTopic(t"beta") should be(Set.empty)
  }

  behavior of "Constraints partition"

  it should "put slot-level constraints apart from the other ones" in {
    problem.slotLevelConstraints should be(Set(
      TopicsSimultaneous(Set(t"alpha", t"gamma")),
      TopicsNotSimultaneous(Set(t"gamma", t"delta").toArraySet)
    ))
    problem.globalLevelConstraints should be(Set.empty)
    problem.softcodedConstraints should be(empty)
  }

  behavior of "Preferences partition"

  it should "separate preferences by level and personal or impersonal" in {
    problem.personalPreferencesListByPerson(p"Arnold").map(_.getClass) should be(List(classOf[PersonTopicPreference]))
    problem.personalPreferencesListByPerson(p"Bianca") should be(Nil)
    problem.impersonalRecordLevelPreferences should be(Set.empty)
    problem.impersonalSlotLevelPreferences should be(Set.empty)
    problem.impersonalGlobalLevelPreferences.map(_.getClass) should be(Set(
      classOf[TopicDirectPreference], classOf[TopicsLinked], classOf[PersonFreeSlotPreference]
    ))
    problem.hasGlobalPreferencesWherePersonsMatter should be(true)
  }

}
