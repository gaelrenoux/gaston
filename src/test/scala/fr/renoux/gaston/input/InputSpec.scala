package fr.renoux.gaston.input

import fr.renoux.gaston.TestUtils.*
import fr.renoux.gaston.model.*
import fr.renoux.gaston.model.Counts.given
import fr.renoux.gaston.model.constraints.*
import fr.renoux.gaston.model.preferences.{PersonTopicPreference, TopicsExclusive, TopicsLinked}
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
      lazy val b: Slot = Slot(1, "B", persons.all, None, maxTopics = Int.MaxValue)
      lazy val c: Slot = Slot(2, "C", persons.all, None, maxTopics = Int.MaxValue)
      val all: Set[Slot] = Set(a, b, c)
    }

    object topics {
      lazy val alpha: Topic = Topic(0, "alpha", min = 5, max = 5, mandatory = Set(persons.bernard))
      lazy val beta: Topic = Topic(1, "beta", min = 4, max = 5, forbidden = Set(persons.laverne), slots = Some(Set(slots.a)))
      lazy val gamma1: Topic = Topic(2, "gamma ~1", min = 4, max = 6, followup = Some(gamma2))
      lazy val gamma2: Topic = Topic(3, "gamma ~2", min = 4, max = 6, isFollowup = true)
      val unassignedA: Topic = Topic.unassigned(4, expected.slots.a)
      val unassignedB: Topic = Topic.unassigned(5, expected.slots.b)
      val unassignedC: Topic = Topic.unassigned(6, expected.slots.c)
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
    val initialTopicsPreferences = for {
      t <- Set(expected.topics.unassignedA, expected.topics.unassignedB, expected.topics.unassignedC)
      p <- Set(expected.persons.bernard, expected.persons.hoagie, expected.persons.laverne)
    } yield PersonTopicPreference(p, t, Score(-1000))
    val additionalPreferences = Set(
      PersonTopicPreference(expected.persons.bernard, expected.topics.alpha, Score(scalingFactor * 5.0)),
      PersonTopicPreference(expected.persons.bernard, expected.topics.beta, Score(scalingFactor * 1.0)),
      PersonTopicPreference(expected.persons.bernard, expected.topics.gamma1, Score(scalingFactor * 1.0)),
      PersonTopicPreference(expected.persons.bernard, expected.topics.gamma2, Score(scalingFactor * 1.0)),
      TopicsExclusive(Set(expected.topics.beta, expected.topics.gamma1).toArraySet, Set(expected.persons.laverne).toArraySet),
      TopicsLinked(Set(expected.topics.gamma1, expected.topics.gamma2).toArraySet)
    )
    val expectedPreferences = initialTopicsPreferences ++ additionalPreferences

    problem.preferences.filter {
      case p: PersonTopicPreference if p.topic.isUnassigned => true
      case _ => false
    } should be(expectedPreferences.filter {
      case p: PersonTopicPreference if p.topic.isUnassigned => true
      case _ => false
    })

    problem.preferences.filter(_.isInstanceOf[PersonTopicPreference]) should be(expectedPreferences.filter(_.isInstanceOf[PersonTopicPreference]))
    problem.preferences.filter(_.isInstanceOf[TopicsExclusive]) should be(expectedPreferences.filter(_.isInstanceOf[TopicsExclusive]))
    problem.preferences should be(expectedPreferences)
  }

}
