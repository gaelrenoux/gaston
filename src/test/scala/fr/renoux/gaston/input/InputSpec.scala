package fr.renoux.gaston.input

import fr.renoux.gaston.TestUtils.*
import fr.renoux.gaston.model.*
import fr.renoux.gaston.model.Counts.given
import fr.renoux.gaston.model.constraints.*
import fr.renoux.gaston.model.preferences.{PersonTopicPreference, PersonUnassignedPreference, TopicsExclusive, TopicsExclusiveFor, TopicsLinked}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InputSpec extends AnyFlatSpec with Matchers {
  // TODO That and InputTranscriptionSpec are redundant, merge them

  val problem: Problem = problemFromClassPath("rendering-test").force

  object expected {

    object persons {
      val bernard: Person = Person(0, "bernard", Weight.Default)
      val laverne: Person = Person(1, "laverne", Weight(1))
      val hoagie: Person = Person(2, "hoagie", Weight(1.5))
      val all: Set[Person] = Set(bernard, laverne, hoagie)
    }

    object slots {
      lazy val a: Slot = Slot(0, "A", persons.all, Some(b), maxTopics = 4)
      lazy val b: Slot = Slot(1, "B", persons.all, None, maxTopics = 5)
      lazy val c: Slot = Slot(2, "C", persons.all, None, maxTopics = 5)
      val all: Set[Slot] = Set(a, b, c)
    }

    object topics {
      val unassignedA: Topic = Topic.unassigned(0, expected.slots.a)
      val unassignedB: Topic = Topic.unassigned(1, expected.slots.b)
      val unassignedC: Topic = Topic.unassigned(2, expected.slots.c)
      lazy val alpha: Topic = Topic(3, "alpha", min = 5, max = 5, mandatory = Set(persons.bernard))
      lazy val beta: Topic = Topic(4, "beta", min = 4, max = 5, forbidden = Set(persons.laverne), slots = Some(Set(slots.a)))
      lazy val gamma1: Topic = Topic(5, "gamma ~1", min = 4, max = 6, followup = Some(gamma2))
      lazy val gamma2: Topic = Topic(6, "gamma ~2", min = 4, max = 6, isFollowup = true)
      val all: Set[Topic] = Set(unassignedA, unassignedB, unassignedC, alpha, beta, gamma1, gamma2)
    }

    given Counts = Counts(slots = 3, topics = 7, persons = 3)
  }

  "Produced problem" should "contain the correct slots" in {
    problem.slotsSet should be(expected.slots.all)
  }

  it should "contain the correct topics" in {
    problem.topicsSet.map(t => (t.id, t.name)).toSeq.sorted should be(expected.topics.all.map(t => (t.id, t.name)).toSeq.sorted)
    problem.topicsSet.toSeq.sortBy(_.name) should be(expected.topics.all.toSeq.sortBy(_.name))
    problem.topicsSet should be(expected.topics.all)
  }

  it should "contain the correct persons" in {
    problem.personsSet should be(expected.persons.all)
  }

  it should "contain the correct constraints" in {
    problem.constraints should be(Set(
      TopicsSimultaneous(Set(expected.topics.alpha, expected.topics.beta)),
    ))
  }

  it should "contain the correct preferences" in {
    import expected.given
    val scalingFactor: Double = Constants.PersonTotalScore.value / 7
    val expectedPersonUnassignedPreferences = expected.persons.all.map { person =>
      PersonUnassignedPreference(person, 0, 0, Score(-10), Score(-200), Score(-600))
    }
    val expectedPreferences = expectedPersonUnassignedPreferences ++ Set(
      PersonTopicPreference(expected.persons.bernard, expected.topics.alpha, Score(scalingFactor * 5.0)),
      PersonTopicPreference(expected.persons.bernard, expected.topics.beta, Score(scalingFactor * 1.0)),
      PersonTopicPreference(expected.persons.bernard, expected.topics.gamma1, Score(scalingFactor * 1.0)),
      PersonTopicPreference(expected.persons.bernard, expected.topics.gamma2, Score(scalingFactor * 1.0)),
      TopicsExclusive(Set(expected.topics.beta, expected.topics.gamma1).toArraySet, Set(expected.persons.laverne).toArraySet),
      TopicsExclusiveFor(Set(expected.topics.alpha, expected.topics.gamma1).toArraySet, Set(expected.persons.hoagie).toArraySet),
      TopicsLinked(Set(expected.topics.gamma1, expected.topics.gamma2).toArraySet)
    )

    val preferencesToCheck = problem.preferences.map {
      case pup: PersonUnassignedPreference => pup.copy(expectedCount = 0) // not checking that in that test
      case p => p
    }

    preferencesToCheck.filter(_.isInstanceOf[PersonTopicPreference]) should be(expectedPreferences.filter(_.isInstanceOf[PersonTopicPreference]))
    preferencesToCheck.filter(_.isInstanceOf[PersonUnassignedPreference]) should be(expectedPreferences.filter(_.isInstanceOf[PersonUnassignedPreference]))
    preferencesToCheck.filter(_.isInstanceOf[TopicsExclusive]) should be(expectedPreferences.filter(_.isInstanceOf[TopicsExclusive]))
    preferencesToCheck should be(expectedPreferences)
  }

}
