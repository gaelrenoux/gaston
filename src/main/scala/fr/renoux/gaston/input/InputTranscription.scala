package fr.renoux.gaston.input

import eu.timepit.refined.auto._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.refineV
import eu.timepit.refined.types.string.NonEmptyString
import fr.renoux.gaston.model._
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.impl.ProblemImpl
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference, TopicDirectPreference, TopicsExclusive}
import fr.renoux.gaston.util.CanGroupToMap.ops._
import fr.renoux.gaston.util.CollectionImplicits._
import scalaz.syntax.validation._
import scalaz.{NonEmptyList, Validation}

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
        .filter { t => t.min.lazyZip(t.max).exists(_ > _) }
        .map { t => s"Topic [${t.name}]: Min (${t.min}) is higher than max (${t.max})" }
    } ++ {
      input.topics
        .filter { t => t.name.contains(Topic.MultipleMarker) || t.name.contains(Topic.OccurrenceMarker) }
        .map { t => s"Topic [${t.name}]: Name cannot contain characters '${Topic.MultipleMarker}' or '${Topic.OccurrenceMarker}'" }
    } ++ {
      input.topics.flatMap { t =>
        val badSlots = t.slots.getOrElse(Set.empty).filter(s => !slotsSet.exists(_.name == s)).map(s => s"[$s]")
        if (badSlots.isEmpty) None
        else Some(s"Topic [${t.name}]: undefined slots: ${badSlots.mkString(", ")}")
      }
    } ++ {
      input.persons.flatMap { p =>
        val badSlots = p.absences.filter(!slotsByName.contains(_)).map(s => s"[$s]")
        if (badSlots.isEmpty) None
        else Some(s"Person [${p.name}]: undefined absence slots: ${badSlots.mkString(", ")}")
      }
    } ++ {
      input.persons.flatMap { p =>
        val badTopics = p.mandatory.filter(!topicsByName.contains(_)).map(t => s"[$t]")
        if (badTopics.isEmpty) None
        else Some(s"Person [${p.name}]: undefined mandatory topics: ${badTopics.mkString(", ")}")
      }
    } ++ {
      input.persons.flatMap { p =>
        val badTopics = p.forbidden.filter(!topicsByName.contains(_)).map(t => s"[$t]")
        if (badTopics.isEmpty) None
        else Some(s"Person [${p.name}]: undefined forbidden topics: ${badTopics.mkString(", ")}")
      }
    } ++ {
      input.persons.flatMap { p =>
        val badTopics = p.wishes.keys.map(refineV[NonEmpty](_)).collect {
          case Left(_) => "[]" // empty name
          case Right(t) if !topicsByName.contains(t) => s"[$t]"
        }
        if (badTopics.isEmpty) None
        else Some(s"Person [${p.name}]: undefined wished topics: ${badTopics.mkString(", ")}")
      }
    } ++ {
      input.persons.flatMap { p =>
        val badPersons = p.incompatible.filter(!personsByName.contains(_)).map(p => s"[$p]")
        if (badPersons.isEmpty) None
        else Some(s"Person [${p.name}]: undefined incompatible persons: ${badPersons.mkString(", ")}")
      }
    } ++ {
      input.constraints.exclusive
        .flatMap(_.topics)
        .filter(!topicsByName.contains(_))
        .map(t => s"Exclusive constraint: unknown topic: [$t]")
    } ++ {
      input.constraints.exclusive
        .flatMap(_.exemptions)
        .filter(!personsByName.contains(_))
        .map(p => s"Exclusive constraint: unknown person: [$p]")
    } ++ {
      input.constraints.simultaneous
        .flatMap(_.topics)
        .filter(!topicsByName.contains(_))
        .map(t => s"Simultaneous constraint: unknown topic: [$t]")
    } ++ {
      input.constraints.simultaneous
        .flatMap(_.topics)
        .filter(t => topicsByName(t).size > 1)
        .map(t => s"Simultaneous constraint: topic [$t]: can't be multiple or have several occurrences")
    } ++ {
      input.constraints.notSimultaneous
        .flatMap(_.topics)
        .filter(!topicsByName.contains(_))
        .map(t => s"SNot-simultaneous constraint: unknown topic: [$t]")
    }

  /* Persons */
  lazy val personsByName: Map[NonEmptyString, Person] = input.persons.map { p => p.name -> Person(p.name, p.weight) }.toMap

  /* Slots */
  lazy val slotSequencesWithNames: Seq[Seq[(NonEmptyString, Slot)]] = input.slots.mapMap { s =>
    val personsPresent = input.personsSet.filterNot(_.absences.contains(s.name)).map(p => personsByName(p.name))
    s.name -> Slot(s.name, personsPresent, s.maxTopics.fold(Int.MaxValue)(_.value))
  }
  lazy val slotSequences: Seq[Seq[Slot]] = slotSequencesWithNames.mapMap(_._2)
  lazy val slotsByName: Map[NonEmptyString, Slot] = slotSequencesWithNames.flatten.toMap
  lazy val slotsSet: Set[InputSlot] = input.slots.flatten.toSet

  /* Topics */
  lazy val baseTopics: List[Topic] = input.topics.map { inTopic =>
    val mandatory = input.personsSet.filter(_.mandatory.contains(inTopic.name)).map(_.name).map(personsByName)
    val forbidden = input.personsSet.filter(_.forbidden.contains(inTopic.name)).map(_.name).map(personsByName)
    val min = inTopic.min.getOrElse(settings.defaultMinPersonsPerTopic)
    val max = inTopic.max.getOrElse(settings.defaultMaxPersonsPerTopic)
    val slots = inTopic.slots.map(ss => ss.map(slotsByName))
    Topic(inTopic.name, mandatory = mandatory, forbidden = forbidden, min = min, max = max, slots = slots)
  }

  lazy val topicsByName: Map[NonEmptyString, Set[Topic]] = input.topics.map { inTopic =>
    val mandatory = input.personsSet.filter(_.mandatory.contains(inTopic.name)).map(_.name).map(personsByName)
    val forbidden = input.personsSet.filter(_.forbidden.contains(inTopic.name)).map(_.name).map(personsByName)
    val min = inTopic.min.getOrElse(settings.defaultMinPersonsPerTopic)
    val max = inTopic.max.getOrElse(settings.defaultMaxPersonsPerTopic)
    val slots = inTopic.slots.map(ss => ss.map(slotsByName))
    val baseTopic = Topic(inTopic.name, mandatory = mandatory, forbidden = forbidden, min = min, max = max, slots = slots, forced = inTopic.forced)

    /* Duplicate topics if more than one occurrence of the topic */
    val occurringTopics = inTopic.forcedOccurrences.value match {
      case 1 => Set(baseTopic)
      case c: Int => baseTopic.occurrences(c).toSet
    }

    /* Duplicate topics if topic is multiple. Must be done second, so that he multiple marker appears after the occurence one. */
    val duplicatedTopics = inTopic.forcedMultiple.value match {
      case 1 => occurringTopics
      case c: Int if c > 0 => occurringTopics.flatMap { topic =>
        val instances = topic.multiple(c)
        val sortedMandatories = baseTopic.mandatory.toSeq.sortBy(_.name) // sorted to be deterministic, therefore more testable

        /* dispatch the mandatory persons on the instances */
        val mandatoriesByInstance = sortedMandatories.zip(LazyList.continually(instances).flatten).map(_.swap).groupToMap
        instances.map { t => t.copy(mandatory = mandatoriesByInstance.getOrElse(t, Nil).toSet) }
      }
    }

    inTopic.name -> duplicatedTopics

  }.toMap

  /* Topics, but with multiple ones (not occurences, just multi-topics!) grouped */
  lazy val topicGroupsByName: Map[NonEmptyString, Set[Set[Topic]]] =
    topicsByName.mapValuesStrict(_.groupBy(_.name.split(Topic.MultipleMarker).head).values.toSet)

  lazy val topics: Set[Topic] = topicsByName.values.toSet.flatten

  /* Constraints */
  object Constraints {
    lazy val simultaneousTopics: Set[TopicsSimultaneous] =
      input.constraints.simultaneous.map { inConstraint =>
        // TODO Better handling of simultaneous and occurrences is needed. Will return an error above
        TopicsSimultaneous(inConstraint.topics.map(topicsByName(_).head))
      }

    lazy val simultaneousMultiple: Set[TopicsSimultaneous] =
      input.multipleTopicsSet.flatMap { inTopic =>
        topicGroupsByName(inTopic.name).map(TopicsSimultaneous)
      }

    lazy val notSimultaneousTopics: Set[TopicsNotSimultaneous] =
      input.constraints.notSimultaneous.map { inConstraint =>
        val topics = inConstraint.topics.flatMap(topicGroupsByName(_)).flatMap(_.headOption) // Keep only one element for multiple topics
        TopicsNotSimultaneous(topics)
      }

    lazy val all: Set[Constraint] =
      simultaneousTopics ++
        simultaneousMultiple
  }

  /* Preferences */
  object Preferences {

    lazy val topicScores: Set[TopicDirectPreference] = for {
      inTopic <- input.topicsSet
      topic <- topicsByName(inTopic.name)
      presenceScore <- inTopic.presence
    } yield TopicDirectPreference(topic, presenceScore)

    lazy val exclusiveTopics: Set[TopicsExclusive] =
      input.constraints.exclusive.map { inConstraint =>
        TopicsExclusive(inConstraint.topics.flatMap(topicsByName), inConstraint.exemptions.map(personsByName))
      }

    lazy val exclusiveOccurrencesAndMultiples: Set[TopicsExclusive] =
      input.topicsSet
        .filter { t =>
          t.occurrences.exists(_ > 1) || t.multiple.exists(_ > 1)
        }
        .map { inTopic =>
          val topics = topicsByName(inTopic.name)
          val mandatoryPersons = input.personsSet.filter(_.mandatory.contains(inTopic.name)).map(ip => personsByName(ip.name))
          TopicsExclusive(topics, mandatoryPersons)
        }

    lazy val groupDislikes: Set[PersonGroupAntiPreference] =
      input.personsSet.collect {
        case ip: InputPerson if ip.incompatible.nonEmpty =>
          val person = personsByName(ip.name)
          val group = ip.incompatible.map(personsByName)
          PersonGroupAntiPreference(person, group, settings.incompatibilityAntiPreference)
      }

    /** Person wishes are scaled so that everyone has the same maximum score. This avoids the problem where someone puts
      * few preferences or with low value only, where he would always stay "unhappy" and therefore privileged when
      * improving the schedule. Right now, we do not handle negative preferences well. */
    lazy val personTopicPreferences: Set[PersonTopicPreference] =
      for {
        inPerson <- input.personsSet
        person = personsByName(inPerson.name)
        totalInputScore = inPerson.wishes.filter(_._2.value > 0).values.sum.value // TODO Right now, negative prefs are ignored in the total count
        scoreFactor = Score.PersonTotalScore.value / totalInputScore
        inWish <- inPerson.wishes
        wishedTopicName <- NonEmptyString.from(inWish._1).toOption.toSet[NonEmptyString]
        topic <- topicsByName(wishedTopicName)
      } yield PersonTopicPreference(person, topic, inWish._2 * scoreFactor)

    lazy val all: Set[Preference] =
      topicScores ++
        exclusiveTopics ++
        exclusiveOccurrencesAndMultiples ++
        groupDislikes ++
        personTopicPreferences
  }

  object Unassigned {
    private lazy val dummies = slotsByName.values.map { s =>
      val topic = Topic.unassigned(s)
      val antiPreferences = personsByName.values.map(PersonTopicPreference(_, topic, Score.PersonTotalScore.negative))
      (topic, antiPreferences)
    }

    lazy val topics: Set[Topic] = dummies.map(_._1).toSet
    lazy val preferences: Set[PersonTopicPreference] = dummies.flatMap(_._2).toSet
  }

  object Nothing {
    lazy val enabled: Boolean =
      settings.maxPersonsOnNothing > 0 && settings.maxPersonsOnNothing >= settings.minPersonsOnNothing

    private lazy val elements: Iterable[(Topic, Iterable[PersonTopicPreference])] =
      slotsByName.values.map { s =>
        val topic = Topic.nothing(s, settings.minPersonsOnNothing, settings.maxPersonsOnNothing)
        val antiPreferences = personsByName.values.map(PersonTopicPreference(_, topic, settings.personOnNothingAntiPreference))
        (topic, antiPreferences)
      }

    lazy val topics: Set[Topic] = if (enabled) elements.map(_._1).toSet else Set.empty
    lazy val preferences: Set[PersonTopicPreference] = if (enabled) elements.flatMap(_._2).toSet else Set.empty
  }

  lazy val problem: Problem =
    new ProblemImpl(
      slotSequences,
      topicsByName.values.flatten.toSet ++ Unassigned.topics ++ Nothing.topics,
      personsByName.values.toSet,
      Constraints.all,
      Preferences.all ++ Unassigned.preferences ++ Nothing.preferences
    )

  lazy val result: Validation[InputErrors, Problem] = errors.toList.sorted.map(InputError(_)) match {
    case Nil => problem.success
    case h :: q => NonEmptyList.fromSeq(h, q).failure
  }

}
