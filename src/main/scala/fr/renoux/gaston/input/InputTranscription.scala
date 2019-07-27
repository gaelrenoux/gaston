package fr.renoux.gaston.input

import eu.timepit.refined.auto._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.refineV
import eu.timepit.refined.types.string.NonEmptyString
import fr.renoux.gaston.model._
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.impl.ProblemImpl
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference, TopicsExclusive}
import fr.renoux.gaston.util.CanGroupToMap._
import fr.renoux.gaston.util.CollectionImplicits._
import scalaz.{NonEmptyList, Validation}
import scalaz.syntax.validation._

/** Converts the Input object to the Problem object. */
private[input] class InputTranscription(input: InputModel) {

  val settings: InputSettings = input.settings

  lazy val errors: Set[String] =
    Set.empty[String] ++ {
      if (input.settings.defaultMinPersonsPerTopic <= input.settings.defaultMaxPersonsPerTopic) None
      else Some(s"Settings: default min persons per topic (${input.settings.defaultMinPersonsPerTopic}) " +
        s"is higher than default max persons per topic (${input.settings.defaultMaxPersonsPerTopic}) ")
    } ++ {
      if (input.settings.minPersonsOnNothing <= input.settings.maxPersonsOnNothing) None
      else Some(s"Settings: Min persons on nothing (${input.settings.minPersonsOnNothing}) " +
        s"is higher than max persons on nothing (${input.settings.maxPersonsOnNothing})")
    } ++ {
      input.topics
        .filter { t => (t.min, t.max).zipped.exists(_ > _) }
        .map { t => s"Topic [${t.name}]: Min (${t.min}) is higher than max (${t.max})" }
    } ++ {
      input.topics.flatMap { t =>
        val badSlots = t.slots.getOrElse(Set.empty).filter(s => !slotsSet.exists(_.name == s)).map(s => s"[$s]")
        if (badSlots.isEmpty) None
        else Some(s"Topic [${t.name}]: undefined slots: ${badSlots.mkString(", ")}")
      }
    } ++ {
      input.persons.flatMap { p =>
        val badSlots = p.absences.filter(!slotsPerName.contains(_)).map(s => s"[$s]")
        if (badSlots.isEmpty) None
        else Some(s"Person [${p.name}]: undefined absence slots: ${badSlots.mkString(", ")}")
      }
    } ++ {
      input.persons.flatMap { p =>
        val badTopics = p.mandatory.filter(!topicsPerName.contains(_)).map(t => s"[$t]")
        if (badTopics.isEmpty) None
        else Some(s"Person [${p.name}]: undefined mandatory topics: ${badTopics.mkString(", ")}")
      }
    } ++ {
      input.persons.flatMap { p =>
        val badTopics = p.forbidden.filter(!topicsPerName.contains(_)).map(t => s"[$t]")
        if (badTopics.isEmpty) None
        else Some(s"Person [${p.name}]: undefined forbidden topics: ${badTopics.mkString(", ")}")
      }
    } ++ {
      input.persons.flatMap { p =>
        val badTopics = p.wishes.keys.map(refineV[NonEmpty](_).right.get).filter(!topicsPerName.contains(_)).map(t => s"[$t]")
        if (badTopics.isEmpty) None
        else Some(s"Person [${p.name}]: undefined wished topics: ${badTopics.mkString(", ")}")
      }
    } ++ {
      input.persons.flatMap { p =>
        val badPersons = p.incompatible.filter(!personsPerName.contains(_)).map(p => s"[$p]")
        if (badPersons.isEmpty) None
        else Some(s"Person [${p.name}]: undefined incompatible persons: ${badPersons.mkString(", ")}")
      }
    } ++ {
      input.constraints.exclusive
        .flatMap(_.topics)
        .filter(!topicsPerName.contains(_))
        .map(t => s"Exclusive constraint: unknown topic: [$t]")
    } ++ {
      input.constraints.exclusive
        .flatMap(_.exemptions)
        .filter(!personsPerName.contains(_))
        .map(p => s"Exclusive constraint: unknown person: [$p]")
    } ++ {
      input.constraints.simultaneous
        .flatMap(_.topics)
        .filter(!topicsPerName.contains(_))
        .map(t => s"Simultaneous constraint: unknown topic: [$t]")
    } ++ {
      input.constraints.simultaneous
        .flatMap(_.topics)
        .filter(t => topicsPerName(t).size > 1)
        .map(t => s"Simultaneous constraint: topic [$t]: can't be multiple or have several occurrences")
    }

  /* Slots */
  lazy val slotSequencesWithNames: Seq[Seq[(NonEmptyString, Slot)]] = input.slots.mapMap(s => s.name -> Slot(s.name))
  lazy val slotSequences: Seq[Seq[Slot]] = slotSequencesWithNames.mapMap(_._2)
  lazy val slotsPerName: Map[NonEmptyString, Slot] = slotSequencesWithNames.flatten.toMap
  lazy val slotsSet: Set[InputSlot] = input.slots.flatten.toSet

  /* Topics */
  lazy val topicsPerName: Map[NonEmptyString, Set[Topic]] = input.topics.map { inTopic =>
    val mandatory = input.personsSet.filter(_.mandatory.contains(inTopic.name)).map(_.name).map(personsPerName)
    val forbidden = input.personsSet.filter(_.forbidden.contains(inTopic.name)).map(_.name).map(personsPerName)
    val min = inTopic.min.getOrElse(settings.defaultMinPersonsPerTopic)
    val max = inTopic.max.getOrElse(settings.defaultMaxPersonsPerTopic)
    val baseTopic = Topic(inTopic.name, mandatory = mandatory, forbidden = forbidden, min = min, max = max)

    /* Demultiply topics if more than one occurence of the topic */
    val occurringTopics = inTopic.forcedOccurrences.value match {
      case 1 => Set(baseTopic)
      case c: Int => baseTopic.occurrences(c).toSet
    }

    /* Demultiply topics if topic is multiple */
    val multipleTopics = inTopic.forcedMultiple.value match {
      case 1 => occurringTopics
      case c: Int if c > 0 => occurringTopics.flatMap { topic =>
        val instances = topic.multiple(c)
        val sortedMandatories = baseTopic.mandatory.toSeq.sortBy(_.name) // sorted to be deterministic, therefore more testable

        /* dispatch the mandatory persons on the instances */
        val mandatoriesByInstance = sortedMandatories.zip(Stream.continually(instances).flatten).map(_.swap).groupToMap
        instances.map { t => t.copy(mandatory = mandatoriesByInstance.getOrElse(t, Nil).toSet) }
      }
    }

    inTopic.name -> multipleTopics

  }.toMap

  /* Topics, but with multiple ones grouped */
  lazy val topicGroupsPerName: Map[NonEmptyString, Set[Set[Topic]]] =
    topicsPerName.mapValuesStrict(_.groupBy(_.name.split(Topic.MultipleMarker).head).values.toSet)

  lazy val topics: Set[Topic] = topicsPerName.values.toSet.flatten

  /* Persons */
  lazy val personsPerName: Map[NonEmptyString, Person] = input.persons.map { p => p.name -> Person(p.name, p.weight) }.toMap

  /* Constraints */
  object Constraints {
    lazy val slotMaxTopicCount: Set[SlotMaxTopicCount] =
      slotsSet.flatMap { inSlot =>
        val slot = slotsPerName(inSlot.name)
        val maxOption = inSlot.maxTopics orElse settings.defaultMaxTopicsPerSlot
        maxOption.map(SlotMaxTopicCount(slot, _))
      }

    lazy val absences: Set[PersonAbsence] =
      input.personsSet.flatMap { ip =>
        ip.absences.map(slotsPerName).map(PersonAbsence(personsPerName(ip.name), _))
      }

    lazy val forcedSlots: Set[TopicForcedSlot] =
      input.topicsSet.flatMap {
        case inTopic: InputTopic if inTopic.slots.nonEmpty =>
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
        // TODO Better handling of simultaneous and occurrences is needed. Will return an error above
        TopicsSimultaneous(inConstraint.topics.map(topicsPerName(_).head))
      }

    lazy val simultaneousMultiple: Set[TopicsSimultaneous] =
      input.topicsSet.filter(_.multiple.exists(_ > 1)).flatMap { inTopic =>
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
      input.topicsSet
        .filter { t =>
          t.occurrences.exists(_ > 1) || t.multiple.exists(_ > 1)
        }
        .map { inTopic =>
          val topics = topicsPerName(inTopic.name)
          val mandatoryPersons = input.personsSet.filter(_.mandatory.contains(inTopic.name)).map(ip => personsPerName(ip.name))
          TopicsExclusive(topics, mandatoryPersons, Score.PersonTotalScore.negative * 100)
        }

    lazy val groupDislikes: Set[PersonGroupAntiPreference] =
      input.personsSet.collect {
        case ip: InputPerson if ip.incompatible.nonEmpty =>
          val person = personsPerName(ip.name)
          val group = ip.incompatible.map(personsPerName)
          PersonGroupAntiPreference(person, group, settings.incompatibilityAntiPreference)
      }

    /** Person wishes are scaled so that everyone has the same maximum score. This avoids the problem where someone puts
      * few preferences or with low value only, where he would always stay "unhappy" and therefore privileged when
      * improving the schedule. Right now, we do not handle negative preferences well. */
    lazy val personTopicPreferences: Set[PersonTopicPreference] =
      for {
        inPerson <- input.personsSet
        person = personsPerName(inPerson.name)
        totalInputScore = inPerson.wishes.filter(_._2.value > 0).values.sum.value // TODO Right now, negative prefs are ignored in the total count
        scoreFactor = Score.PersonTotalScore.value / totalInputScore
        inWish <- inPerson.wishes
        wishedTopicName <- NonEmptyString.from(inWish._1).toOption.toSet[NonEmptyString]
        topic <- topicsPerName(wishedTopicName)
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
      val countConstraint = TopicNeedsNumberOfPersons(topic, min = 0, max = personsPerName.size)
      val slotConstraint = TopicForcedSlot(topic, Set(s))
      val antiPreferences = personsPerName.values.map(PersonTopicPreference(_, topic, Score.PersonTotalScore.negative))
      (topic, Set(slotConstraint, countConstraint), antiPreferences)
    }

    lazy val topics: Set[Topic] = dummies.map(_._1).toSet
    lazy val constraints: Set[Constraint] = dummies.flatMap(_._2).toSet
    lazy val preferences: Set[PersonTopicPreference] = dummies.flatMap(_._3).toSet
  }

  object Nothing {
    lazy val enabled: Boolean =
      settings.maxPersonsOnNothing > 0 && settings.maxPersonsOnNothing >= settings.minPersonsOnNothing

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

  lazy val problem: Problem =
    new ProblemImpl(
      slotSequences,
      topicsPerName.values.flatten.toSet ++ Unassigned.topics ++ Nothing.topics,
      personsPerName.values.toSet,
      Constraints.all ++ Unassigned.constraints ++ Nothing.constraints,
      Preferences.all ++ Unassigned.preferences ++ Nothing.preferences
    )

  lazy val result: Validation[InputErrors, Problem] = errors.toList.sorted.map(InputError(_)) match {
    case Nil => problem.success
    case h :: q => NonEmptyList(h, q: _*).failure
  }

}
