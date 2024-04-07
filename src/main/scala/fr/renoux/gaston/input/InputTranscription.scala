package fr.renoux.gaston.input

import cats.data.{NonEmptyList, ValidatedNel}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import eu.timepit.refined.auto._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.refineV
import eu.timepit.refined.types.string.NonEmptyString
import fr.renoux.gaston.model._
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.impl.ProblemImpl
import fr.renoux.gaston.model.preferences._
import fr.renoux.gaston.util.CanGroupToMap.ops._
import fr.renoux.gaston.util.CollectionImplicits._
import mouse.map._

import java.util.concurrent.atomic.AtomicInteger

// TODO This class needs a test, big time

/** Converts the Input object (canonical input) to the Problem object (internal representation of the problem to
  * optimize).
  *
  * It is split in various fields for clarity and ease of development. To use it, simply instantiate it and get the end
  * result in the `result` field.
  */
private[input] class InputTranscription(input: InputModel) {

  import fr.renoux.gaston.input.InputTranscription._

  private val log = Logger[InputTranscription]

  val settings: InputSettings = input.settings


  /* Checking errors */
  lazy val errors: Set[String] = checkErrors(input)


  /* Persons */
  lazy val personsById: Array[Person] = input.persons.zipWithIndex.map { case (p, ix) => Person(ix, p.name, p.weight, p.baseScore) }.toArray
  lazy val personsByName: Map[NonEmptyString, Person] = personsById.map { p => NonEmptyString.unsafeFrom(p.name) -> p }.toMap


  /* Slots */
  private val slotIx = new AtomicInteger(0) // ugly, simpler
  lazy val slotSequencesWithNames: Seq[Seq[(NonEmptyString, Slot)]] = input.slots.map { inSequence =>
    val slotsWithNoNextSlot = inSequence.map { inSlot =>
      val personsPresent = input.personsSet.filterNot(_.absences.contains(inSlot.name)).map(p => personsByName(p.name))
      inSlot.name -> Slot(slotIx.getAndIncrement(), inSlot.name, personsPresent, None, inSlot.maxTopics.fold(Int.MaxValue)(_.value))
    }
    /* Iterate twice, because we prefer the first one to be in natural order (so that the generated ids follow that order). */
    slotsWithNoNextSlot.reverseIterator.mapWithState(Option.empty[Slot]) { case ((slotName, slot), nextSlot) =>
      val newSlot = slot.copy(next = nextSlot)
      (slotName -> newSlot, Some(newSlot))
    }._1.toList
  }
  lazy val slotSequences: Seq[Seq[Slot]] = slotSequencesWithNames.mapMap(_._2)
  lazy val slotsByName: Map[NonEmptyString, Slot] = slotSequencesWithNames.flatten.toMap


  /* Topics */
  private val topicIx = new AtomicInteger(0) // ugly, simpler
  lazy val unassignedTopicsByNameAndSlot: Map[(NonEmptyString, Slot), Topic] = {
    input.slotsSet.map { inSlot =>
      val slot = slotsByName(inSlot.name)
      val name = unassignedTopicName(inSlot.name)
      (name, slot) -> unassignedTopic(topicIx.getAndIncrement(), slot)
    }.toMap
  }
  lazy val nothingTopicsByName: Map[NonEmptyString, Topic] = if (!input.settings.isNothingEnabled) Map.empty[NonEmptyString, Topic] else {
    log.info("Persons-on-nothing enabled")
    input.slotsSet.map { inSlot =>
      val slot = slotsByName(inSlot.name)
      val name = nothingTopicName(inSlot.name)
      val min = settings.minPersonsOnNothing
      val max = settings.maxPersonsOnNothing
      name -> Topic(topicIx.getAndIncrement(), name, min = min, max = max, slots = Some(Set(slot)), virtual = true)
    }.toMap
  }

  lazy val concreteTopicsByOccurrenceIndexByName: Map[NonEmptyString, Map[Int, Topic]] = {
    input.topics.map { inTopic: InputTopic =>
      val mandatory = input.personsSet.filter(_.mandatory.contains(inTopic.name)).map(_.name).map(personsByName)
      val forbidden = input.personsSet.filter(_.forbidden.contains(inTopic.name)).map(_.name).map(personsByName)
      val min = inTopic.min.getOrElse(settings.defaultMinPersonsPerTopic)
      val max = inTopic.max.getOrElse(settings.defaultMaxPersonsPerTopic)
      val slots = inTopic.slots.map(ss => ss.map(slotsByName))

      val multiplesByOccurrenceIndex: Map[Int, Topic] = inTopic.occurrenceInstances.map { inTopicOccurrence =>
        val topic = Topic(
          topicIx.getAndIncrement(), inTopicOccurrence.name,
          mandatory = mandatory, forbidden = forbidden,
          min = min, max = max,
          slots = slots, forced = inTopic.forced
        )
        inTopicOccurrence.index.getOrElse(0) -> topic
      }.toMap

      // in the preferences section, we will make occurrences incompatible (so that one person does not register to the several occurrences)
      inTopic.name -> multiplesByOccurrenceIndex
    }.toMap
  }

  lazy val topicsByOccurrenceIndexByName: Map[NonEmptyString, Map[Int, Topic]] =
    concreteTopicsByOccurrenceIndexByName ++
      nothingTopicsByName.mapValuesStrict { topic => Map(0 -> topic) } ++
      unassignedTopicsByNameAndSlot.map { case (key, topic) => key._1 -> Map(0 -> topic) }

  lazy val topicsByName: Map[NonEmptyString, Set[Topic]] = topicsByOccurrenceIndexByName.mapValuesStrict(_.values.toSet)


  /* Counts */
  lazy val slotsCount: Int = slotsByName.size
  lazy val personsCount: Int = personsByName.size
  lazy val topicsCount: Int = topicsByName.values.flatten.size
  implicit lazy val counts: Counts = Counts(slots = slotsCount, topics = topicsCount, persons = personsCount)


  /* Constraints */
  object Constraints {
    lazy val simultaneousTopics: Set[TopicsSimultaneous] =
      input.constraints.simultaneous.flatMap { inConstraint =>
        // We already verified that all topics in the constraint have the same number of occurrences => match them one-to-one
        val topicsByOccurrenceIndex: Map[Int, Set[Topic]] = inConstraint.topics.flatMap(topicsByOccurrenceIndexByName(_)).groupToMap
        topicsByOccurrenceIndex.values.map(TopicsSimultaneous)
      }

    lazy val notSimultaneousTopics: Set[TopicsNotSimultaneous] =
      input.constraints.notSimultaneous.map { inConstraint =>
        TopicsNotSimultaneous(inConstraint.topics.flatMap(topicsByName).toBitSet)
      }

    // TODO Merge simultaneous constraint (ex: Sim(1, 2) and Sim(2, 3) can be merged into Sim(1, 2, 3))

    lazy val all: Set[Constraint] = {
      Set.empty[Constraint] ++ // force the correct type
        simultaneousTopics ++
        notSimultaneousTopics
    }
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
        TopicsExclusive(inConstraint.topics.flatMap(topicsByName).toBitSet, inConstraint.exemptions.map(personsByName).toBitSet)
      }

    lazy val exclusiveOccurrences: Set[TopicsExclusive] =
      topicsByOccurrenceIndexByName.values.view.filter(_.size > 1).map { reoccurringTopic: Map[Int, Topic] =>
        val mandatoryPersons = reoccurringTopic.head._2.mandatory // mandatories are the same on all instances, take the first one
        TopicsExclusive(reoccurringTopic.values.toBitSet, mandatoryPersons.toBitSet)
      }.toSet

    lazy val linkedTopics: Set[TopicsLinked] =
      input.constraints.linked.map { inConstraint =>
        TopicsLinked(inConstraint.topics.flatMap(topicsByName).toBitSet)
      }

    lazy val groupDislikes: Set[PersonGroupAntiPreference] =
      input.personsSet.collect {
        case ip: InputPerson if ip.incompatible.nonEmpty =>
          val person = personsByName(ip.name)
          val group = ip.incompatible.map(personsByName)
          PersonGroupAntiPreference(person, group.toBitSet, settings.incompatibilityAntiPreference)
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

    def getNothingAntiPreference(inPerson: InputPerson): Score = settings.personOnNothingAntiPreferenceScaling match {
      case None => settings.personOnNothingAntiPreference
      case Some(scalingSettings) if !scalingSettings.enabled => settings.personOnNothingAntiPreference
      case Some(scalingSettings) =>
        val forbiddenRatio = inPerson.forbidden.size.toDouble / input.topics.size
        val antiPreferenceVariablePart: Score = settings.personOnNothingAntiPreference - scalingSettings.maximumAntiPreference
        val antiPreferenceRatio = math.max(0, 1 - (forbiddenRatio / scalingSettings.forbiddenRatioForMaximum))
        scalingSettings.maximumAntiPreference + (antiPreferenceVariablePart * antiPreferenceRatio)
    }

    lazy val nothingTopicPreferences: Set[PersonTopicPreference] = {
      for {
        nothingTopic <- nothingTopicsByName.values
        inPerson <- input.personsSet
        person = personsByName(inPerson.name)
      } yield PersonTopicPreference(person, nothingTopic, getNothingAntiPreference(inPerson))
    }.toSet

    lazy val unassignedTopicPreferences: Set[PersonTopicPreference] = {
      for {
        unassignedTopic <- unassignedTopicsByNameAndSlot.values
        person <- personsByName.values
      } yield PersonTopicPreference(person, unassignedTopic, Score.PersonTotalScore.negative)
    }.toSet

    lazy val all: Set[Preference] = {
      Set.empty[Preference] ++
        topicScores ++
        exclusiveTopics ++
        exclusiveOccurrences ++
        groupDislikes ++
        personTopicPreferences ++
        nothingTopicPreferences ++
        unassignedTopicPreferences
    }
  }



  /* Construction of the Problem */
  lazy val problem: Problem = {
    val p = new ProblemImpl(
      slotSequences,
      topicsByName.values.flatten.toSet,
      unassignedTopicsByNameAndSlot.mapKeys(_._2).toBitMap(),
      personsByName.values.toSet,
      Constraints.all,
      Preferences.all
    )
    log.debug(p.toFormattedString)
    p
  }

  lazy val result: ValidatedNel[InputError, Problem] = errors.toList.sorted.map(InputError(_)) match {
    case Nil => problem.valid
    case h :: q => NonEmptyList.of(h, q: _*).invalid[Problem]
  }

}

object InputTranscription {

  private val VirtualTopicPrefix = "@"

  def nothingTopicName(slotName: String): NonEmptyString = NonEmptyString.unsafeFrom(s"${VirtualTopicPrefix}Nothing ($slotName)")

  def unassignedTopicName(slotName: String): NonEmptyString = NonEmptyString.unsafeFrom(s"${VirtualTopicPrefix}[$slotName]")

  def unassignedTopic(id: Int, slot: Slot): Topic = Topic(id, unassignedTopicName(slot.name), max = Person.MaxCount, slots = Some(Set(slot)), virtual = true)

  private def checkErrors(input: InputModel): Set[String] = {
    Set.empty[String] ++
      checkSettingsErrors(input) ++
      checkSlotErrors(input) ++
      checkTopicErrors(input) ++
      checkPersonErrors(input) ++
      checkConstraintErrors(input)
  }

  private def checkSettingsErrors(input: InputModel): Set[String] = {
    Set.empty[String] ++ {
      if (input.settings.defaultMinPersonsPerTopic <= input.settings.defaultMaxPersonsPerTopic) None
      else Some(s"Settings: default min persons per topic (${input.settings.defaultMinPersonsPerTopic}) " +
        s"is higher than default max persons per topic (${input.settings.defaultMaxPersonsPerTopic}) ")
    } ++ {
      if (input.settings.minPersonsOnNothing <= input.settings.maxPersonsOnNothing) None
      else Some(s"Settings: Min persons on nothing (${input.settings.minPersonsOnNothing}) " +
        s"is higher than max persons on nothing (${input.settings.maxPersonsOnNothing})")
    }
  }

  private def checkSlotErrors(input: InputModel): Set[String] = {
    Set.empty[String] ++ {
      val duplicates = input.slots.flatten.groupBy(_.name).mapValuesStrict(_.size).filter(_._2 > 1).keySet
      duplicates.map { d => s"Duplicate slot name: $d" }
    }
  }

  private def checkTopicErrors(input: InputModel): Set[String] = {
    Set.empty[String] ++ {
      val duplicates = input.topics.groupBy(_.name).mapValuesStrict(_.size).filter(_._2 > 1).keySet
      duplicates.map { d => s"Duplicate topic name: $d" }
    } ++ {
      input.topics
        .filter { t => t.name.startsWith(VirtualTopicPrefix) }
        .map { t => s"Topic [${t.name}]: prefix $VirtualTopicPrefix is reserved by the software" }
    } ++ {
      input.topics
        .filter { t => t.min.lazyZip(t.max).exists(_ > _) }
        .map { t => s"Topic [${t.name}]: Min (${t.min}) is higher than max (${t.max})" }
    } ++ {
      input.topics
        .filter { t => t.name.contains(InputTopic.MultipleMarker) || t.name.contains(InputTopic.OccurrenceMarker) }
        .map { t => s"Topic [${t.name}]: Name cannot contain characters '${InputTopic.MultipleMarker}' or '${InputTopic.OccurrenceMarker}'" }
    } ++ {
      input.topics.flatMap { t =>
        val badSlots = t.slots.getOrElse(Set.empty).filter(s => !input.slotsSet.exists(_.name == s)).map(s => s"[$s]")
        if (badSlots.isEmpty) None
        else Some(s"Topic [${t.name}]: undefined slots: ${badSlots.mkString(", ")}")
      }
    }
  }

  private def checkPersonErrors(input: InputModel): Set[String] = {
    Set.empty[String] ++ {
      val duplicates = input.persons.groupBy(_.name).mapValuesStrict(_.size).filter(_._2 > 1).keySet
      duplicates.map { d => s"Duplicate person name: $d" }
    } ++ {
      input.persons.flatMap { p =>
        val badSlots = p.absences.filter(!input.slotsNameSet.contains(_)).map(s => s"[$s]")
        if (badSlots.isEmpty) None
        else Some(s"Person [${p.name}]: undefined absence slots: ${badSlots.mkString(", ")}")
      }
    } ++ {
      input.persons.flatMap { p =>
        val badTopics = p.mandatory.filter(!input.topicsNameSet.contains(_)).map(t => s"[$t]")
        if (badTopics.isEmpty) None
        else Some(s"Person [${p.name}]: undefined mandatory topics: ${badTopics.mkString(", ")}")
      }
    } ++ {
      input.persons.flatMap { p =>
        val badTopics = p.forbidden.filter(!input.topicsNameSet.contains(_)).map(t => s"[$t]")
        if (badTopics.isEmpty) None
        else Some(s"Person [${p.name}]: undefined forbidden topics: ${badTopics.mkString(", ")}")
      }
    } ++ {
      input.persons.flatMap { p =>
        val badTopics = p.wishes.keys.map(refineV[NonEmpty](_)).collect {
          case Left(_) => "[]" // empty name
          case Right(t) if !input.topicsNameSet.contains(t) => s"[$t]"
        }
        if (badTopics.isEmpty) None
        else Some(s"Person [${p.name}]: undefined wished topics: ${badTopics.mkString(", ")}")
      }
    } ++ {
      input.persons.flatMap { p =>
        val badPersons = p.incompatible.filter(!input.personsNameSet.contains(_)).map(p => s"[$p]")
        if (badPersons.isEmpty) None
        else Some(s"Person [${p.name}]: undefined incompatible persons: ${badPersons.mkString(", ")}")
      }
    }
  }

  private def checkConstraintErrors(input: InputModel): Set[String] = {
    Set.empty[String] ++ {
      input.constraints.exclusive
        .flatMap(_.topics)
        .filter(!input.topicsNameSet.contains(_))
        .map(t => s"Exclusive constraint: unknown topic: [$t]")
    } ++ {
      input.constraints.exclusive
        .flatMap(_.exemptions)
        .filter(!input.personsNameSet.contains(_))
        .map(p => s"Exclusive constraint: unknown person: [$p]")
    } ++ {
      input.constraints.linked
        .flatMap(_.topics)
        .filter(!input.topicsNameSet.contains(_))
        .map(t => s"Linked constraint: unknown topic: [$t]")
    } ++ {
      input.constraints.simultaneous
        .flatMap(_.topics)
        .filter(!input.topicsNameSet.contains(_))
        .map(t => s"Simultaneous constraint: unknown topic: [$t]")
    } ++ {
      input.constraints.simultaneous
        .flatMap { inConstraint =>
          val topics = inConstraint.topics.flatMap(input.topicsByName.get)
          if (topics.map(_.forcedOccurrences).toSet.size > 1) {
            Some(s"Simultaneous constraint: different occurrence count: [${inConstraint.topics.mkString(", ")}]")
          } else None
        }
    } ++ {
      input.constraints.notSimultaneous
        .flatMap(_.topics)
        .filter(!input.topicsNameSet.contains(_))
        .map(t => s"SNot-simultaneous constraint: unknown topic: [$t]")
    }
  }

}
