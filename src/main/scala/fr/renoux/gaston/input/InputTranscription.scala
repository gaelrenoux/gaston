package fr.renoux.gaston.input

import fr.renoux.gaston.model._
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference, TopicsExclusive}
import fr.renoux.gaston.model.impl.ProblemImpl
import scalaz.Validation
import scalaz.syntax.validation._
import fr.renoux.gaston.util.CollectionImplicits._
import fr.renoux.gaston.util.CanGroupToMap._

/** Converts the Input object to the Problem object. */
class InputTranscription(input: InputModel) {

  val settings: InputSettings = input.settings

  /* Slots */
  lazy val slotSequences: Seq[Seq[Slot]] = input.slots.map(_.map(s => Slot(s.name)))
  lazy val slotsPerName: Map[String, Slot] = slotSequences.flatten.map(s => s.name -> s).toMap

  /* Topics */
  lazy val topicsPerName: Map[String, Set[Topic]] = input.topics.map { inTopic =>
    val mandatory = input.persons.filter(_.mandatory.contains(inTopic.name)).map(_.name).map(personsPerName)
    val forbidden = input.persons.filter(_.forbidden.contains(inTopic.name)).map(_.name).map(personsPerName)
    val min = inTopic.min.getOrElse(settings.defaultMinPersonsPerTopic)
    val max = inTopic.max.getOrElse(settings.defaultMaxPersonsPerTopic)
    val baseTopic = Topic(inTopic.name, mandatory = mandatory, forbidden = forbidden, min = min, max = max)

    val occurringTopics = inTopic.forcedOccurrences match {
      case 1 => Set(baseTopic)
      case c if c > 0 => baseTopic.occurrences(c).toSet
      case _ => throw new IllegalArgumentException("Can't have a non-positive number of occurrences") //TODO use Refined instead
    }

    val multipleTopics = inTopic.forcedMultiple match {
      case 1 => occurringTopics
      case c if c > 0 => occurringTopics.flatMap { topic =>
        val groups = topic.multiple(c)
        val sortedMandatories = baseTopic.mandatory.toSeq.sortBy(_.name) //sorted to be deterministic, therefore more testable

        /* dispatch the mandatory persons on the groups */
        val mandatoriesByGroup = sortedMandatories.zip(Stream.continually(groups).flatten).map(_.swap).groupToMap
        groups.map { t => t.copy(mandatory = mandatoriesByGroup.getOrElse(t, Nil).toSet) }
      }
      case _ => throw new IllegalArgumentException("Can't have a non-positive number of multiples") //TODO use Refined instead
    }

    inTopic.name -> multipleTopics

  }.toMap

  /* Topics, but with multiple ones grouped */
  lazy val topicGroupsPerName: Map[String, Set[Set[Topic]]] = topicsPerName.mapValuesStrict(_.groupBy(_.name.split(Topic.MultipleMarker).head).values.toSet)

  lazy val topics: Set[Topic] = topicsPerName.values.toSet.flatten

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
        topic <- topics
        person <- topic.mandatory
      } yield PersonTopicObligation(person, topic)

    lazy val interdictions: Set[PersonTopicInterdiction] =
      for {
        topic <- topics
        person <- topic.forbidden
      } yield PersonTopicInterdiction(person, topic)

    lazy val numbers: Set[TopicNeedsNumberOfPersons] =
      topics.map { topic => TopicNeedsNumberOfPersons(topic, topic.min, topic.max) }


    lazy val simultaneousTopics: Set[TopicsSimultaneous] =
      input.constraints.simultaneous.map { inConstraint =>
        //TODO Better handling of simultaneous and occurrences is needed, should at least return an error
        TopicsSimultaneous(inConstraint.topics.map(topicsPerName(_).head))
      }

    lazy val simultaneousMultiple: Set[TopicsSimultaneous] =
      input.topics.filter(_.multiple.exists(_ > 1)).flatMap { inTopic =>
        topicGroupsPerName(inTopic.name).map(TopicsSimultaneous)
      }

    lazy val all: Set[Constraint] =
      slotMaxTopicCount ++
        absences ++
        forcedSlots ++
        obligations ++
        interdictions ++
        numbers ++
        simultaneousTopics ++
        simultaneousMultiple
  }

  /* Preferences */
  object Preferences {
    lazy val exclusiveTopics: Set[TopicsExclusive] =
      input.constraints.exclusive.map { inConstraint =>
        TopicsExclusive(inConstraint.topics.flatMap(topicsPerName), inConstraint.exemptions.map(personsPerName), Score.PersonTotalScore.negative * 100)
      }

    lazy val exclusiveOccurrencesAndMultiples: Set[TopicsExclusive] =
      input.topics
        .filter { t =>
          t.occurrences.exists(_ > 1) || t.multiple.exists(_ > 1)
        }
        .map { inTopic =>
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
        exclusiveOccurrencesAndMultiples
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
  def apply(input: InputModel): InputTranscription = new InputTranscription(input)
}