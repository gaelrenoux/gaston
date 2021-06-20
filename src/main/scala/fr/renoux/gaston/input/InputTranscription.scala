package fr.renoux.gaston.input

import cats.data.{NonEmptyList, ValidatedNel}
import cats.implicits._
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
import mouse.map._

import java.util.concurrent.atomic.AtomicInteger

/** Converts the Input object to the Problem object. */
private[input] class InputTranscription(input: InputModel) {

  import fr.renoux.gaston.input.InputTranscription._

  val settings: InputSettings = input.settings


  /* Checking errors */
  lazy val errors: Set[String] = checkErrors(input)


  /* Persons */
  lazy val personsById: Array[Person] = input.persons.zipWithIndex.map { case (p, ix) => Person(ix, p.name, p.weight) }.toArray
  lazy val personsByName: Map[NonEmptyString, Person] = personsById.map { p => NonEmptyString.unsafeFrom(p.name) -> p }.toMap


  /* Slots */
  private val slotIx = new AtomicInteger(0) // ugly, simpler
  lazy val slotSequencesWithNames: Seq[Seq[(NonEmptyString, Slot)]] = input.slots.mapMap { s =>
    val personsPresent = input.personsSet.filterNot(_.absences.contains(s.name)).map(p => personsByName(p.name))
    s.name -> Slot(slotIx.getAndIncrement(), s.name, personsPresent, s.maxTopics.fold(Int.MaxValue)(_.value))
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
    input.slotsSet.map { inSlot =>
      val slot = slotsByName(inSlot.name)
      val name = nothingTopicName(inSlot.name)
      val min = settings.minPersonsOnNothing
      val max = settings.maxPersonsOnNothing
      name -> Topic(topicIx.getAndIncrement(), name, min = min, max = max, slots = Some(Set(slot)), virtual = true)
    }.toMap
  }

  lazy val concreteTopicMultiplesByOccurrenceIndexByName: Map[NonEmptyString, Map[Int, Set[Topic]]] = {
    input.topics.map { inTopic: InputTopic =>
      val mandatory = input.personsSet.filter(_.mandatory.contains(inTopic.name)).map(_.name).map(personsByName)
      val forbidden = input.personsSet.filter(_.forbidden.contains(inTopic.name)).map(_.name).map(personsByName)
      val min = inTopic.min.getOrElse(settings.defaultMinPersonsPerTopic)
      val max = inTopic.max.getOrElse(settings.defaultMaxPersonsPerTopic)
      val slots = inTopic.slots.map(ss => ss.map(slotsByName))

      val multiplesByOccurrenceIndex: Map[Int, Set[Topic]] =
        inTopic.occurrenceInstances.map { inTopicOccurrence =>
          val topicParts = inTopicOccurrence.multipleParts

          /* dispatch the mandatory persons on the instances. The map may be missing parts if there are not enough mandatory persons. */
          val sortedMandatories = mandatory.toSeq.sortBy(_.name) // sorted to be deterministic, therefore more testable
          val mandatoriesByPart = sortedMandatories.zip(LazyList.continually(topicParts).flatten).map(_.swap).groupToMap

          val topics = topicParts.map { part => // can't iterate on mandatoriesByPart, may be missing parts
            val thisMandatories = mandatoriesByPart.getOrElse(part, Seq.empty)
            Topic(
              topicIx.getAndIncrement(), part.name,
              mandatory = thisMandatories.toSet, forbidden = forbidden,
              min = min, max = max,
              slots = slots, forced = inTopic.forced
            )
          }.toSet

          // in the constraint section, we will make parts simultaneous
          inTopicOccurrence.index.getOrElse(0) -> topics
        }.toMap

      // in the preferences section, we will make occurrences incompatible (so that one person does not register to the several occurrences)
      inTopic.name -> multiplesByOccurrenceIndex
    }.toMap
  }

  lazy val topicMultiplesByOccurrenceIndexByName: Map[NonEmptyString, Map[Int, Set[Topic]]] =
    concreteTopicMultiplesByOccurrenceIndexByName ++
      nothingTopicsByName.mapValuesStrict { topic => Map(0 -> Set(topic)) } ++
      unassignedTopicsByNameAndSlot.map { case (key, topic) => key._1 -> Map(0 -> Set(topic)) }

  lazy val topicsByName: Map[NonEmptyString, Set[Topic]] = topicMultiplesByOccurrenceIndexByName.mapValuesStrict(_.values.flatten.toSet)


  /* Counts */
  lazy val slotsCount: Int = slotsByName.size
  lazy val personsCount: Int = personsByName.size
  lazy val topicsCount: Int = topicsByName.values.flatten.size
  implicit lazy val counts: Counts = Counts(slots = slotsCount, topics = topicsCount, persons = personsCount)


  /* Constraints */
  object Constraints {
    lazy val simultaneousTopics: Set[TopicsSimultaneous] =
      input.constraints.simultaneous.map { inConstraint =>
        // Taking the first occurrence only (weird, but that's a weird requirement as well). Taking the first part only, as all parts are simultaneous anyway.
        TopicsSimultaneous(inConstraint.topics.map(topicMultiplesByOccurrenceIndexByName(_).head._2.head))
      }

    lazy val simultaneousMultipleParts: Set[TopicsSimultaneous] = {
      for {
        topicMultiplesByOccurrenceIndex <- topicMultiplesByOccurrenceIndexByName.values
        topicMultiples <- topicMultiplesByOccurrenceIndex.values
        if topicMultiples.size > 1
      } yield TopicsSimultaneous(topicMultiples)
    }.toSet

    lazy val notSimultaneousTopics: Set[TopicsNotSimultaneous] =
      input.constraints.notSimultaneous.map { inConstraint =>
        val inConstraintTopicOccurrences = inConstraint.topics.flatMap { topicName =>
          topicMultiplesByOccurrenceIndexByName(topicName).values
        }.map(_.head) // Keep only one element for multiple topics
        TopicsNotSimultaneous(inConstraintTopicOccurrences.toBitSet)
      }

    // TODO Merge simultaneous constraint (ex: Sim(1, 2) and Sim(2, 3) can be merged into Sim(1, 2, 3))

    lazy val all: Set[Constraint] = {
      Set.empty[Constraint] ++ // force the correct type
        simultaneousTopics ++
        simultaneousMultipleParts ++
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
      topicMultiplesByOccurrenceIndexByName.values.view.filter(_.size > 1).map { reoccurringTopic: Map[Int, Set[Topic]] =>
        val mandatoryPersons = reoccurringTopic.head._2.head.mandatory // mandatories are the same on all instances, take the first one
        val allInstancesPart = reoccurringTopic.values.flatten // all instances exclusive (obvious for the multiple parts, but only one constraint is better)
        TopicsExclusive(allInstancesPart.toBitSet, mandatoryPersons.toBitSet)
      }.toSet

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

    lazy val nothingTopicPreferences: Set[PersonTopicPreference] = {
      for {
        nothingTopic <- nothingTopicsByName.values
        person <- personsByName.values
      } yield PersonTopicPreference(person, nothingTopic, settings.personOnNothingAntiPreference)
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
  lazy val problem: Problem =
    new ProblemImpl(
      slotSequences,
      topicsByName.values.flatten.toSet,
      unassignedTopicsByNameAndSlot.mapKeys(_._2).toBitMap(),
      personsByName.values.toSet,
      Constraints.all,
      Preferences.all
    )

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
      input.constraints.simultaneous
        .flatMap(_.topics)
        .filter(!input.topicsNameSet.contains(_))
        .map(t => s"Simultaneous constraint: unknown topic: [$t]")
    } ++ {
      input.constraints.notSimultaneous
        .flatMap(_.topics)
        .filter(!input.topicsNameSet.contains(_))
        .map(t => s"SNot-simultaneous constraint: unknown topic: [$t]")
    }
  }

}
