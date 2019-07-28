package fr.renoux.gaston.input

import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.{PersonTopicPreference, TopicsExclusive}
import fr.renoux.gaston.model.{Slot, _}
import org.scalatest.{FlatSpec, Matchers}

class InputSpec extends FlatSpec with Matchers {

  val problem: Problem = problemFromClassPath("test-application").force

  object expected {

    object persons {
      val bernard = Person("bernard", Weight.Default)
      val laverne = Person("laverne", Weight(1))
      val hoagie = Person("hoagie", Weight(1.5))
      val all: Set[Person] = Set(bernard, laverne, hoagie)
    }

    object slots {
      val a = Slot("A", persons.all, maxTopics = 4)
      val b = Slot("B", persons.all, maxTopics = Int.MaxValue)
      val all: Set[Slot] = Set(a, b)
    }

    object topics {
      val unassignedA: Topic = Topic.unassigned(expected.slots.a)
      val unassignedB: Topic = Topic.unassigned(expected.slots.b)
      val alpha = Topic("alpha", min = 5, max = 5, mandatory = Set(Person("bernard", Weight.Default)))
      val beta = Topic("beta", min = 4, max = 5, forbidden = Set(Person("laverne", Weight.Default)), slots = Some(Set(slots.a)))
      val gamma = Topic("gamma", min = 4, max = 6)
      val all: Set[Topic] = Set(unassignedA, unassignedB, alpha, beta, gamma)
    }

  }

  "Produced problem" should "contain the correct slots" in {
    problem.slots should be(expected.slots.all)
  }

  it should "contain the correct topics" in {
    problem.topics.toSeq.sortBy(_.name) should be(expected.topics.all.toSeq.sortBy(_.name))
    problem.topics should be(expected.topics.all)
  }

  it should "contain the correct persons" in {
    problem.persons should be(expected.persons.all)
  }

  it should "contain the correct constraints" in {
    problem.constraints should be(Set(
      TopicsSimultaneous(Set(expected.topics.alpha, expected.topics.beta)),
    ))
  }

  it should "contain the correct preferences" in {
    val scalingFactor: Double = Score.PersonTotalScore.value / 7
    val initialTopicsPreferences = for {
      t <- Set(Topic.unassigned(expected.slots.a), Topic.unassigned(expected.slots.b))
      p <- Set(expected.persons.bernard, expected.persons.hoagie, expected.persons.laverne)
    } yield PersonTopicPreference(p, t, Score(-1000))
    problem.preferences should be(initialTopicsPreferences ++ Set(
      PersonTopicPreference(expected.persons.bernard, expected.topics.alpha, Score(scalingFactor * 5.0)),
      PersonTopicPreference(expected.persons.bernard, expected.topics.beta, Score(scalingFactor * 1.0)),
      PersonTopicPreference(expected.persons.bernard, expected.topics.gamma, Score(scalingFactor * 1.0)),
      TopicsExclusive(Set(expected.topics.beta, expected.topics.gamma), Set(expected.persons.laverne))
    ))
  }

}
