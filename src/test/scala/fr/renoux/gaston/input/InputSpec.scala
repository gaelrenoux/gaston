package fr.renoux.gaston.input

import java.io.File

import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.model.{Slot, _}
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.{PersonTopicPreference, TopicsExclusive}
import org.scalatest.{FlatSpec, Matchers}

class InputSpec extends FlatSpec with Matchers {

  "Loading from default" should "load the default input when no name is given" in {
    val input = InputLoader.fromDefault.force
    input.gaston.settings.incompatibilityAntiPreference should be(Score(-1000))
    input.gaston.persons.size should be(3)
  }

  "Loading from the classpath" should "load the correct input" in {
    val input = InputLoader.fromClassPath("named-configuration.conf").force
    input.gaston.settings.incompatibilityAntiPreference should be(Score(-1042))
    input.gaston.persons.size should be(1)
  }

  "Loading from a file" should "load the correct input" in {
    val stringPath = getClass.getResource("/named-configuration.conf").getPath
    val path = new File(stringPath).toPath
    val input = InputLoader.fromPath(path).force
    input.gaston.settings.incompatibilityAntiPreference should be(Score(-1042))
    input.gaston.persons.size should be(1)
  }

  "Checking the sample" should "work" in {
    InputLoader.fromClassPath("sample.conf").force
  }


  val problem: Problem = problemFromDefault.force

  object expected {

    object slots {
      val a = Slot("A")
      val b = Slot("B")
      val all: Set[Slot] = Set(a, b)
    }

    object topics {
      val unassignedA: Topic = Topic.unassigned(expected.slots.a)
      val unassignedB: Topic = Topic.unassigned(expected.slots.b)
      val alpha = Topic("alpha", min = 5, max = 5, mandatory = Set(Person("bernard", Weight.Default)))
      val beta = Topic("beta", min = 4, max = 5, forbidden = Set(Person("laverne", Weight.Default)))
      val gamma = Topic("gamma", min = 4, max = 6)
      val all: Set[Topic] = Set(unassignedA, unassignedB, alpha, beta, gamma)
    }

    object persons {
      val bernard = Person("bernard", Weight.Default)
      val laverne = Person("laverne", Weight(1))
      val hoagie = Person("hoagie", Weight(1.5))
      val all: Set[Person] = Set(bernard, laverne, hoagie)
    }

  }

  "Produced problem" should "contain the correct slots" in {
    problem.slots should be(expected.slots.all)
  }

  it should "contain the correct topics" in {
    problem.topics should be(expected.topics.all)
  }

  it should "contain the correct persons" in {
    problem.persons should be(expected.persons.all)
  }

  it should "contain the correct constraints" in {
    problem.constraints should be(Set(
      SlotMaxTopicCount(expected.slots.a, 4),
      SlotMaxTopicCount(expected.slots.b, 5),
      TopicForcedSlot(Topic.unassigned(expected.slots.a), Set(expected.slots.a)),
      TopicForcedSlot(Topic.unassigned(expected.slots.b), Set(expected.slots.b)),
      TopicNeedsNumberOfPersons(Topic.unassigned(expected.slots.a), 0, 3),
      TopicNeedsNumberOfPersons(Topic.unassigned(expected.slots.b), 0, 3),
      PersonTopicObligation(expected.persons.bernard, expected.topics.alpha),
      PersonTopicInterdiction(expected.persons.laverne, expected.topics.beta),
      TopicNeedsNumberOfPersons(expected.topics.alpha, 5, 5),
      TopicNeedsNumberOfPersons(expected.topics.gamma, 4, 6),
      TopicNeedsNumberOfPersons(expected.topics.beta, 4, 5),
      TopicForcedSlot(expected.topics.beta, Set(expected.slots.a)),
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
