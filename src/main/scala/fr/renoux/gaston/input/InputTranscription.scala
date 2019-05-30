package fr.renoux.gaston.input

import fr.renoux.gaston.model._
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference, TopicsExclusive}
import fr.renoux.gaston.model.problem.ProblemImpl
import scalaz.Validation
import scalaz.syntax.validation._

/** Converts the Input object to the Problem object. */
class InputTranscription(inputRoot: InputRoot) {

  val input: InputModel = inputRoot.gaston
  val settings: InputSettings = input.settings

  /* Slots */
  lazy val slotSequences: Seq[Seq[Slot]] = input.slots.map(_.map(s => Slot(s.name)))
  lazy val slotsPerName: Map[String, Slot] = slotSequences.flatten.map(s => s.name -> s).toMap

  /* Topics */
  lazy val topicsPerName: Map[String, Set[Topic]] = input.topics.map { inTopic =>
    val mandatory =  input.persons.filter(_.mandatory.contains(inTopic.name)).map(_.name).map(personsPerName)
    val forbidden = input.persons.filter(_.forbidden.contains(inTopic.name)).map(_.name).map(personsPerName)
    val min = inTopic.min.getOrElse(settings.defaultMinPersonsPerTopic)
    val max = inTopic.max.getOrElse(settings.defaultMaxPersonsPerTopic)
    val baseTopic = Topic(inTopic.name, mandatory = mandatory, forbidden = forbidden, min = min, max = max)

    inTopic.forcedOccurrences match {
      case 1 => inTopic.name -> Set(baseTopic)
      case c if c > 0 => inTopic.name -> baseTopic.duplicates(c)
      case _ => throw new IllegalArgumentException("Can't have a negative number of occurrences") //TODO use Refined instead
    }
  }.toMap

  /* Persons */
  lazy val personsPerName: Map[String, Person] = input.persons.map { p => p.name -> Person(p.name, p.weight) }.toMap

  /* Constraints */
  object Constraints {
    lazy val slotMaxTopicCount: Set[SlotMaxTopicCount] =
      input.slots.flatten.toSet[InputSlot].flatMap { inSlot =>
        val slot = slotsPerName(inSlot.name)
        val maxOption = inSlot.maxTopics orElse settings.defaultMaxTopicsPerSlot
        maxOption.map(SlotMaxTopicCount(slot, _))
      }

    lazy val absences: Set[PersonAbsence] =
      input.persons.flatMap { ip =>
        ip.absences.map(slotsPerName).map(PersonAbsence(personsPerName(ip.name), _))
      }

    lazy val forcedSlots: Set[TopicForcedSlot] =
      input.topics.flatMap {
        case inTopic if inTopic.slots.nonEmpty =>
          val topics = topicsPerName(inTopic.name)
          val slots = inTopic.slots.get.map(slotsPerName)
          topics.map(TopicForcedSlot(_, slots))
        case _ => Set.empty[TopicForcedSlot]
      }

    lazy val obligations: Set[PersonTopicObligation] =
      for {
        ip <- input.persons
        person = personsPerName(ip.name)
        topicName <- ip.mandatory
        topic <- topicsPerName(topicName)
      } yield PersonTopicObligation(person, topic)

    lazy val interdictions: Set[PersonTopicInterdiction] =
      for {
        ip <- input.persons
        person = personsPerName(ip.name)
        topicName <- ip.forbidden
        topic <- topicsPerName(topicName)
      } yield PersonTopicInterdiction(person, topic)

    lazy val numbers: Set[TopicNeedsNumberOfPersons] =
      for {
        inTopic <- input.topics
        topic <- topicsPerName(inTopic.name)
        min = inTopic.min.getOrElse(settings.defaultMinPersonsPerTopic)
        max = inTopic.max.getOrElse(settings.defaultMaxPersonsPerTopic)
      } yield TopicNeedsNumberOfPersons(topic, min, max)


    lazy val simultaneousTopics: Set[TopicsSimultaneous] =
      input.constraints.simultaneous.map { inConstraint =>
        //TODO Better handling of simultaneous and multiple is needed, should at least return an error
        TopicsSimultaneous(inConstraint.topics.map(topicsPerName(_).head))
      }

    lazy val all: Set[Constraint] =
      slotMaxTopicCount ++
        absences ++
        forcedSlots ++
        obligations ++
        interdictions ++
        numbers ++
        simultaneousTopics
  }

  /* Preferences */
  object Preferences {
    lazy val exclusiveTopics: Set[TopicsExclusive] =
      input.constraints.exclusive.map { inConstraint =>
        TopicsExclusive(inConstraint.topics.flatMap(topicsPerName), inConstraint.exemptions.map(personsPerName), Score.PersonTotalScore.negative * 100)
      }

    lazy val exclusiveOccurrences: Set[TopicsExclusive] =
      input.topics.filter(_.occurrences.exists(_ > 1)).map { inTopic =>
        val topics = topicsPerName(inTopic.name)
        val mandatoryPersons = input.persons.filter(_.mandatory.contains(inTopic.name)).map(ip => personsPerName(ip.name))
        TopicsExclusive(topics, mandatoryPersons, Score.PersonTotalScore.negative * 100)
      }

    lazy val groupDislikes: Set[PersonGroupAntiPreference] =
      input.persons.collect {
        case ip if ip.incompatible.nonEmpty =>
          val person = personsPerName(ip.name)
          val group = ip.incompatible.map(personsPerName)
          PersonGroupAntiPreference(person, group, settings.incompatibilityAntiPreference)
      }

    /** Person wishes are scaled so that everyone has the same maximum score. This avoids the problem where someone puts
      * few preferences or with low value only, where he would always stay "unhappy" and therefore privileged when
      * improving the schedule. Right now, we do not handle negative preferences well. */
    lazy val personTopicPreferences: Set[PersonTopicPreference] =
      for {
        inPerson <- input.persons
        person = personsPerName(inPerson.name)
        totalInputScore = inPerson.wishes.filter(_._2.value > 0).values.sum.value //TODO Right now, negative prefs are ignored in the total count
        scoreFactor = Score.PersonTotalScore.value / totalInputScore
        inWish <- inPerson.wishes
        topic <- topicsPerName(inWish._1)
      } yield PersonTopicPreference(person, topic, inWish._2 * scoreFactor)

    lazy val all: Set[Preference] =
      groupDislikes ++
        personTopicPreferences ++
        exclusiveTopics ++
        exclusiveOccurrences
  }

  object Unassigned {
    private lazy val dummies = slotsPerName.values.map { s =>
      val topic = Topic.unassigned(s)
      val countConstraint = TopicNeedsNumberOfPersons(topic, 0, personsPerName.size)
      val slotConstraint = TopicForcedSlot(topic, Set(s))
      val antiPreferences = personsPerName.values.map(PersonTopicPreference(_, topic, Score.PersonTotalScore.negative))
      (topic, Set(slotConstraint, countConstraint), antiPreferences)
    }

    lazy val topics: Set[Topic] = dummies.map(_._1).toSet
    lazy val constraints: Set[Constraint] = dummies.flatMap(_._2).toSet
    lazy val preferences: Set[PersonTopicPreference] = dummies.flatMap(_._3).toSet
  }

  object Nothing {
    lazy val enabled: Boolean = settings.maxPersonsOnNothing > 0 &&
      settings.maxPersonsOnNothing >= settings.minPersonsOnNothing

    private lazy val elements: Iterable[(Topic, TopicNeedsNumberOfPersons, TopicForcedSlot, Iterable[PersonTopicPreference])] =
      slotsPerName.values.map { s =>
        val topic = Topic.nothing(s)
        val countConstraint = TopicNeedsNumberOfPersons(topic, settings.minPersonsOnNothing, settings.maxPersonsOnNothing)
        val slotConstraint = TopicForcedSlot(topic, Set(s))
        val antiPreferences = personsPerName.values.map(PersonTopicPreference(_, topic, settings.personOnNothingAntiPreference))
        (topic, countConstraint, slotConstraint, antiPreferences)
      }

    lazy val topics: Set[Topic] = if (enabled) elements.map(_._1).toSet else Set.empty
    lazy val constraints: Set[Constraint] = if (enabled) (elements.map(_._2) ++ elements.map(_._3)).toSet else Set.empty
    lazy val preferences: Set[PersonTopicPreference] = if (enabled) elements.flatMap(_._4).toSet else Set.empty
  }

  lazy val problem: Validation[InputErrors, Problem] =
    new ProblemImpl(
      slotSequences,
      topicsPerName.values.flatten.toSet ++ Unassigned.topics ++ Nothing.topics,
      personsPerName.values.toSet,
      Constraints.all ++ Unassigned.constraints ++ Nothing.constraints,
      Preferences.all ++ Unassigned.preferences ++ Nothing.preferences
    ).success

}

object InputTranscription {
  def apply(inputRoot: InputRoot): InputTranscription = new InputTranscription(inputRoot)
}