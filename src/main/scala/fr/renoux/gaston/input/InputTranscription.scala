package fr.renoux.gaston.input

import cats.data.{NonEmptyList, ValidatedNel}
import cats.implicits.*
import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.*
import fr.renoux.gaston.model.Counts.given
import fr.renoux.gaston.model.constraints.*
import fr.renoux.gaston.model.preferences.*
import fr.renoux.gaston.util.*
import fr.renoux.gaston.util.CanGroupToMap.given
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.{Constraint as _, *}
import mouse.map.*

import java.util.concurrent.atomic.AtomicInteger

/** Converts the Input object (canonical input) to the Problem object (internal representation of the problem to
 * optimize).
 *
 * It is split in various fields for clarity and ease of development. To use it, simply instantiate it and get the end
 * result in the `result` field.
 */
private[input] final class InputTranscription(rawInput: InputModel) {

  import fr.renoux.gaston.input.InputTranscription.*

  private val log = Logger[InputTranscription]

  // val input: InputModel = rawInput.clean // TODO Restore cleaner here
  val input: InputModel = rawInput
  val settings: InputSettings = input.settings

  /* Checking errors */
  lazy val errors: Set[String] = checkErrors(input)


  /* Persons */
  lazy val unassignedScoreByPersonId: Map[Person.Id, Score] = input.persons.zipWithIndex.map { case (inPerson, index) =>
    val score: Score = settings.unassigned.personAntiPreferenceScaling match {
      case None => settings.unassigned.personAntiPreference
      case Some(scalingSettings) =>
        val countForbiddenNormal = inPerson.forbidden.count { f => input.normalTopicsSet.exists(_.name == f) }
        val ratioOfForbiddenTopics = countForbiddenNormal.toDouble / input.topics.size
        val antiPreferenceVariablePart: Score = settings.unassigned.personAntiPreference - scalingSettings.maximumAntiPreference
        val ratioOnVariablePart = math.max(0, 1 - (ratioOfForbiddenTopics / scalingSettings.forbiddenRatioForMaximum))
        scalingSettings.maximumAntiPreference + (antiPreferenceVariablePart * ratioOnVariablePart)
    }
    index -> score
  }.toMap
  lazy val personsById: Array[Person] = input.persons.zipWithIndex.map { case (inPerson, index) =>
    // Ugly adjustment: min-free-slots will lead to negative scores for the person, this will revert it
    val baseScoreAdjustment = (unassignedScoreByPersonId(index) * inPerson.minFreeSlots.getOrElse(0)).opposite
    Person(index, inPerson.name, inPerson.weight, inPerson.baseScore + baseScoreAdjustment)
  }.toArray
  lazy val personsByName: Map[NonEmptyString, Person] = personsById.map { p => (p.name.refineUnsafe[Not[Empty]]: NonEmptyString) -> p }.toMap


  /* Slots */
  private val slotIx = new AtomicInteger(0) // ugly, simpler
  lazy val slotSequencesWithNames: Seq[Seq[(NonEmptyString, Slot)]] = input.slots.map { inSequence =>
    // On each sequence: we do a first iteration to set the generated ids in the natural order (the one in the input).
    // Then we do a second pass to set the nextSlot (we need to iterate in reverse for this).
    val slotsBase = inSequence.map { inSlot => // not setting the next-slot yet
      val personsPresent = input.personsSet.filterNot(_.absences.contains(inSlot.name)).map(p => personsByName(p.name))
      inSlot.name -> Slot(slotIx.getAndIncrement(), inSlot.name, personsPresent, None, inSlot.maxTopics.getOrElse(Int.MaxValue))
    }
    slotsBase.reverseIterator.mapWithState(Option.empty[Slot]) { case ((slotName, slot), nextSlot) =>
      val newSlot = slot.copy(next = nextSlot)
      (slotName -> newSlot, Some(newSlot))
    }._1.toList.reverse
  }
  lazy val slotSequences: Seq[Seq[Slot]] = slotSequencesWithNames.mapMap(_._2)
  lazy val slotsByName: Map[NonEmptyString, Slot] = slotSequencesWithNames.flatten.toMap


  /* Topics */
  private val topicIx = new AtomicInteger(0) // ugly, simpler
  lazy val unassignedTopicsByNameAndSlot: Map[(NonEmptyString, Slot), Topic] =
    if (!input.settings.unassigned.allowed) Map.empty[(NonEmptyString, Slot), Topic] else {
      log.info("Unassigned persons are allowed")
      input.slots.flatten.map { inSlot =>
        val slot = slotsByName(inSlot.name)
        val name = Topic.unassignedName(inSlot.name).refineUnsafe[Not[Empty]]
        val topic = Topic.unassigned(topicIx.getAndIncrement(), slot, min = settings.unassigned.minPersons, max = settings.unassigned.maxPersons)
        (name, slot) -> topic
      }.toMap
    }

  lazy val basicTopicsByPartIndexByOccurrenceIndexByName: Map[NonEmptyString, Map[Int, Map[Int, Topic]]] = {
    input.topics.map { (inTopic: InputTopic) =>
      val mandatory = input.personsSet.filter(_.mandatory.contains(inTopic.name)).map(_.name).map(personsByName)
      val forbidden = input.personsSet.filter(_.forbidden.contains(inTopic.name)).map(_.name).map(personsByName)
      val min = inTopic.min.getOrElse(settings.defaultMinPersonsPerTopic)
      val max = inTopic.max.getOrElse(settings.defaultMaxPersonsPerTopic)
      val slots = inTopic.slots.map(ss => ss.map(slotsByName))

      val topicsByIndexes: Map[Int, Map[Int, Topic]] =
        inTopic.occurrenceInstances.reverseIterator.map { inTopicOccurrence =>
          var followup = Option.empty[Topic]
          val topicsByPartIndex: Map[Int, Topic] = inTopicOccurrence.partInstances
            .map { inTopicPart =>
              val topic = Topic(
                topicIx.getAndIncrement(), inTopicPart.name,
                mandatory = mandatory, forbidden = forbidden,
                min = min, max = max,
                slots = slots, forced = inTopic.forced, isFollowup = inTopicPart.index.exists(_ > 1)
              )
              inTopicPart.index.getOrElse(0) -> topic
            }
            // second iteration in reverse to add the followup (first one is straight because we prefer id to be increasing with the parts
            .reverseIterator.map { case (partIndex, topic) =>
              assert(followup.forall(_.isFollowup))
              val topicWithFollowup = topic.copy(followup = followup)
              followup = Some(topicWithFollowup)
              partIndex -> topicWithFollowup
            }
            .toMap
          inTopicOccurrence.index.getOrElse(0) -> topicsByPartIndex
        }.toMap

      // in the preferences section, we will make occurrences incompatible (so that one person does not register to several occurrences of the same input-topic)
      inTopic.name -> topicsByIndexes
    }.toMap
  }

  lazy val topicsByPartIndexByOccurrenceIndexByName: Map[NonEmptyString, Map[Int, Map[Int, Topic]]] =
    basicTopicsByPartIndexByOccurrenceIndexByName ++
      unassignedTopicsByNameAndSlot.map { case (key, topic) => key._1 -> Map(0 -> Map(0 -> topic)) }

  lazy val topicsFirstPartByOccurrenceIndexByName: Map[NonEmptyString, Map[Int, Topic]] =
    topicsByPartIndexByOccurrenceIndexByName.mapValuesStrict { topicsByPartIndexByOccurrenceIndex =>
      topicsByPartIndexByOccurrenceIndex.mapValuesStrict(_.minKeyOption.get)
    }

  lazy val topicsByName: Map[NonEmptyString, Set[Topic]] =
    topicsByPartIndexByOccurrenceIndexByName.mapValuesStrict(_.values.flatMap(_.values).toSet)

  lazy val topicsFirstPartByName: Map[NonEmptyString, Set[Topic]] =
    topicsFirstPartByOccurrenceIndexByName.mapValuesStrict(_.values.toSet)


  /* Counts */
  given Count[Slot] = Count[Slot](slotsByName.size)

  given Count[Person] = Count[Person](personsByName.size)

  given Count[Topic] = Count[Topic](topicsByName.values.flatten.size)


  /* Constraints */
  object Constraints {
    lazy val simultaneousTopics: Set[TopicsSimultaneous] =
      input.constraints.simultaneous.flatMap { inConstraint =>
        // We already verified that all topics in the constraint have the same number of occurrences => match them one-to-one
        // If they are multi-parts, we only need to impose the constraint on the first part
        val topicsByOccurrenceIndex: Map[Int, Set[Topic]] = inConstraint.topics.flatMap { topicName =>
          topicsFirstPartByOccurrenceIndexByName(topicName)
        }.groupToMap
        topicsByOccurrenceIndex.values.map(TopicsSimultaneous.apply)
      }

    lazy val notSimultaneousTopics: Set[TopicsNotSimultaneous] =
      input.constraints.notSimultaneous.map { inConstraint =>
        TopicsNotSimultaneous(inConstraint.topics.flatMap(topicsByName).toArraySet)
      }

    // TODO Merge simultaneous constraint (ex: Sim(1, 2) and Sim(2, 3) can be merged into Sim(1, 2, 3))

    lazy val topicsLimitedCounts: Set[TopicsLimitedCount] =
      input.constraints.limitedCount.map { inConstraint =>
        TopicsLimitedCount(inConstraint.topics.flatMap(topicsByName).toArraySet, inConstraint.count)
      }

    lazy val all: Set[Constraint] = {
      Set.empty[Constraint] ++ // force the correct type
        simultaneousTopics ++
        notSimultaneousTopics ++
        topicsLimitedCounts
    }
  }


  /* Preferences */
  object Preferences {

    lazy val topicScores: Set[TopicDirectPreference] = for {
      inTopic <- input.normalTopicsSet
      topic <- topicsByName(inTopic.name)
      presenceScore <- inTopic.presence
    } yield TopicDirectPreference(topic, presenceScore)

    lazy val exclusiveTopics: Set[Preference.GlobalLevel] =
      input.constraints.exclusive.map { inConstraint =>
        // topic-parts are always linked, so we only need to mark the exclusivity on the first part
        if (inConstraint.inclusions.isEmpty) {
          TopicsExclusive(inConstraint.topics.flatMap(topicsFirstPartByName).toArraySet, inConstraint.forcedExemptions.map(personsByName).toArraySet)
        } else {
          TopicsExclusiveFor(inConstraint.topics.flatMap(topicsFirstPartByName).toArraySet, inConstraint.forcedInclusions.map(personsByName).toArraySet)
        }
      }

    lazy val exclusiveOccurrences: Set[TopicsExclusive] = input.topics.view
      .filter(_.forcedOccurrences > 1)
      .map { inTopic =>
        // Because all topic-parts are linked (same persons on each), we can just check exclusivity on the first topic-part (index 1)
        val topicsByOccurrenceIndex = topicsFirstPartByOccurrenceIndexByName(inTopic.name)
        val mandatoryPersons = topicsByOccurrenceIndex.head._2.mandatory // mandatories are the same on all instances, take the first one
        TopicsExclusive(topicsByOccurrenceIndex.values.toArraySet, mandatoryPersons.toArraySet)
      }
      .toSet

    lazy val linkedTopics: Set[TopicsLinked] =
      input.constraints.linked.map { inConstraint =>
        TopicsLinked(inConstraint.topics.flatMap(topicsByName).toArraySet)
      }

    lazy val linkedParts: Set[TopicsLinked] = input.topics.view
      .filter(_.forcedDuration > 1)
      .flatMap { inTopic => topicsByPartIndexByOccurrenceIndexByName(inTopic.name).values }
      .map { (partsInOccurrence: Map[Int, Topic]) => TopicsLinked(partsInOccurrence.values.toArraySet) }
      .toSet

    lazy val groupDislikes: Set[PersonGroupAntiPreference] =
      input.personsSet.collect {
        case ip: InputPerson if ip.incompatible.nonEmpty =>
          val person = personsByName(ip.name)
          val group = ip.incompatible.map(personsByName)
          PersonGroupAntiPreference(person, group.toArraySet, settings.incompatibilityAntiPreference)
      }

    /** Wishes are scaled so that everyone has the same maximum score. Otherwise, you could put either very small scores
     * (and therefore stay the lowest score in the schedule and therefore privileged when improving), or with such
     * high values that everyone else's preference don't matter any more.
     *
     * We do not handle negative preferences here (they're excluded from the total). */
    // TODO Right now, negative prefs are ignored in the total count. Either handle them or just forbid negative wishes.
    lazy val personScoreNormalFactors: Map[NonEmptyString, Double] = input.personsSet.view.map { inPerson =>
      // Stopgap topic are not considered for calculating the general factor
      val normalTopicWishes = inPerson.ironedWishes.filter { case (name, _) => input.normalTopicsNameSet.contains(name) }
      val totalTopicWishesScore = normalTopicWishes.filter(_._2.value > 0).values.sum.value
      val totalPersonWishesScore = inPerson.personWishes.filter(_._2.value > 0).values.sum.value
      val totalWishScore = totalTopicWishesScore + totalPersonWishesScore
      val personMissedSlots = inPerson.absences.size + inPerson.minFreeSlots.getOrElse(0)
      val absenceRatio = personMissedSlots.toDouble / slotsByName.size
      val adjustedAbsenceRatio = absenceRatio * input.settings.absenceAdjustmentFactor
      val scoreFactor = if (totalWishScore == 0) 0 else Constants.PersonTotalScore.value / totalWishScore / (1 - adjustedAbsenceRatio)
      inPerson.name -> scoreFactor
    }.toMap

    lazy val personScoreStopgapFactors: Map[NonEmptyString, Double] = input.personsSet.view.map { inPerson =>
      // Only considering stopgap topics
      val stopgapTopicWishes = inPerson.ironedWishes.filter { case (name, _) => input.stopgapTopicsNameSet.contains(name) }
      val totalWishScore = stopgapTopicWishes.filter(_._2.value > 0).values.sum.value
      val scoreFactor = if (totalWishScore == 0) 0 else Constants.PersonStopgapTotalScore.value / totalWishScore
      inPerson.name -> scoreFactor
    }.toMap

    lazy val personNormalTopicPreferences: Set[PersonTopicPreference] =
      for {
        inPerson <- input.personsSet
        person = personsByName(inPerson.name)
        scoreFactor = personScoreNormalFactors(inPerson.name)
        (wishName, wishValue) <- inPerson.ironedWishes
        if !input.topicsByName(wishName).forcedStopgap
        topic <- topicsByName(wishName)
        score = wishValue * scoreFactor
        if !score.isZero
      } yield PersonTopicPreference(person, topic, score)

    lazy val personStopgapTopicPreferences: Set[PersonTopicPreference] =
      for {
        inPerson <- input.personsSet
        person = personsByName(inPerson.name)
        unassignedScore = unassignedScoreByPersonId(person.id)
        inTopic <- input.stopgapTopicsSet
        topic <- topicsByName(inTopic.name)
        scoreFactor = personScoreStopgapFactors(inPerson.name)
        wishValue = inPerson.ironedWishes.getOrElse(inTopic.name, Score.Zero)
        score = (wishValue * scoreFactor) + unassignedScore
        if !score.isZero
      } yield PersonTopicPreference(person, topic, score)

    lazy val personTopicPreferences = personNormalTopicPreferences ++ personStopgapTopicPreferences

    lazy val personPersonPreferences: Set[PersonPersonPreference] =
      for {
        inPerson <- input.personsSet
        person = personsByName(inPerson.name)
        scoreFactor = personScoreNormalFactors(inPerson.name)
        (wishName, wishValue) <- inPerson.ironedPersonWishes
        wishedPerson = personsByName(wishName)
      } yield PersonPersonPreference(person, wishedPerson, wishValue * scoreFactor)

    lazy val unassignedTopicPreferences: Set[PersonTopicPreference] = {
      for {
        unassignedTopic <- unassignedTopicsByNameAndSlot.values
        inPerson <- input.personsSet
        person = personsByName(inPerson.name)
      } yield PersonTopicPreference(person, unassignedTopic, unassignedScoreByPersonId(person.id))
    }.toSet

    lazy val unassignedTopicsExclusivePreferences: Set[TopicsExclusive] =
      input.settings.unassigned.personMultipleAntiPreference.fold(Set.empty[TopicsExclusive]) { reward =>
        Set(TopicsExclusive(unassignedTopicsByNameAndSlot.values.toArraySet, ArraySet.empty, reward))
      }

    lazy val personFreeSlotsPreferences: Seq[PersonFreeSlotPreference] =
      input.persons.flatMap { inPerson =>
        val person = personsByName(inPerson.name)
        inPerson.minFreeSlots.map(PersonFreeSlotPreference(person, _))
      }


    lazy val all: Set[Preference] = {
      Set.empty[Preference] ++
        topicScores ++
        exclusiveTopics ++
        exclusiveOccurrences ++
        linkedTopics ++
        linkedParts ++
        groupDislikes ++
        personTopicPreferences ++
        personPersonPreferences ++
        unassignedTopicPreferences ++
        unassignedTopicsExclusivePreferences ++
        personFreeSlotsPreferences
    }
  }


  /* Construction of the Problem */
  lazy val problem: Problem = {
    val p = new Problem(
      slotSequences,
      topicsByName.values.flatten.toSet,
      if (unassignedTopicsByNameAndSlot.isEmpty) ArrayMap.empty else unassignedTopicsByNameAndSlot.mapKeys(_._2).toArrayMap,
      personsByName.values.toSet,
      Constraints.all,
      Preferences.all
    )
    log.debug(p.toFormattedString)
    p
  }

  lazy val result: ValidatedNel[InputError, Problem] = errors.toList.sorted.map(InputError(_)) match {
    case Nil => problem.valid
    case h :: q => NonEmptyList.of(h, q *).invalid[Problem]
  }

}

object InputTranscription {

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
      if (input.settings.unassigned.minPersons <= input.settings.unassigned.maxPersons) None
      else Some(s"Settings: Min persons on unassigned (${input.settings.unassigned.minPersons}) " +
        s"is higher than max persons on unassigned (${input.settings.unassigned.maxPersons})")
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
        .filter { t => t.name.startsWith(Topic.SyntheticPrefix) }
        .map { t => s"Topic [${t.name}]: prefix ${Topic.SyntheticPrefix} is reserved by the software" }
    } ++ {
      input.topics
        .filter { t => t.min.lazyZip(t.max).exists(_ > _) }
        .map { t => s"Topic [${t.name}]: Min (${t.min}) is higher than max (${t.max})" }
    } ++ {
      input.topics
        .filter { t => t.name.contains(InputTopic.PartMarker) || t.name.contains(InputTopic.OccurrenceMarker) }
        .map { t => s"Topic [${t.name}]: Name cannot contain characters '${InputTopic.PartMarker}' or '${InputTopic.OccurrenceMarker}'" }
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
        val badTopics = p.wishes.keys.map(_.refineEither[Not[Empty]]).collect {
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
        .flatMap { c => c.forcedExemptions ++ c.forcedInclusions }
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

  def transcribe(input: InputModel): Either[InputErrors, Problem] = new InputTranscription(input).result.toEither
}
