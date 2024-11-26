package fr.renoux.gaston.input

import cats.data.NonEmptyList
import cats.data.ValidatedNel
import cats.implicits.*
import fr.renoux.gaston.model.{Score as _, Weight as _, *}
import fr.renoux.gaston.model2.*
import fr.renoux.gaston.util.{Count as _, *}
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.{Constraint as _, *}


/** Converts the Input object (canonical input) to the Problem object (internal representation of the problem to
 * optimize).
 *
 * It is split in various fields for clarity and ease of development. To use it, simply instantiate it and get the end
 * result in the `result` field.
 */
private[input] final class InputTranscription2(rawInput: InputModel) {

  import fr.renoux.gaston.input.InputTranscription2.*

  // val input: InputModel = rawInput.clean // TODO Restore cleaner here
  val input: InputModel = rawInput
  val settings: InputSettings = input.settings

  /* Checking errors */
  lazy val errors: Set[String] = checkErrors(input)



  /* Get all counts first */
  lazy given slotsCount: CountAll[SlotId] = CountAll[SlotId](input.slots.flatten.size)
  lazy given topicsCount: CountAll[TopicId] = CountAll[TopicId](slotsCount.value + input.topics.map { inT => inT.forcedOccurrences * inT.forcedDuration }.sum)
  lazy given personsCount: CountAll[PersonId] = CountAll[PersonId](input.persons.size)

  

  /* Persons */
  lazy val personsNames: IdMap[PersonId, String] = IdMap.unsafeFrom[PersonId, String](input.persons.map(_.name: String).toArray)
  lazy val personsWeights: IdMap[PersonId, Weight] = IdMap.unsafeFrom[PersonId, Weight](input.persons.map(_.weight.value: Weight).toArray)
  lazy val personsBaseScores: IdMap[PersonId,Score] = IdMap.unsafeFrom[PersonId, Score](input.persons.map(_.baseScore.value: Score).toArray)
  lazy val personsIdByName: Map[String, PersonId] = personsNames.toReverseMap



  /* Slots */
  private lazy val flattenedSlots = input.slots.flatten
  lazy val slotsNames: IdMap[SlotId, String] = IdMap.unsafeFrom[SlotId, String](flattenedSlots.map(_.name: String).toArray)
  lazy val slotsMaxTopics: IdMap[SlotId, Count[TopicId]] = IdMap.unsafeFrom[SlotId, Count[TopicId]] {
    flattenedSlots.map(s => s.maxTopics.orElse(settings.defaultMaxTopicsPerSlot).getOrElse(Count.maxCount[TopicId])).toArray
  }
  lazy val slotToNextSlot = {
    val result = IdMap.tabulate[SlotId, SlotId](_.value + 1)
    var index = -1
    input.slots.map(_.size).foreach { seqLen =>
      index += seqLen
      result(index) = SlotId.None
    }
    result
  }
  lazy val slotsIdByName: Map[String, SlotId] = slotsNames.toReverseMap



  /* Topics */
  lazy object topics {
    val topicsNames = IdMap.empty[TopicId, String]
    val topicsMandatories = IdMap.fill[TopicId, SmallIdSet[PersonId]](SmallIdSet.empty[PersonId])
    val topicsMin = IdMap.empty[TopicId, Count[PersonId]]
    val topicsMax = IdMap.empty[TopicId, Count[PersonId]]
    val topicsAllowedSlots = IdMap.fill[TopicId, SmallIdSet[SlotId]](SmallIdSet.full[SlotId])
    var topicsForced = SmallIdSet.empty[TopicId]
    val topicsFollowup = IdMap.fill[TopicId, TopicId](TopicId.None)
    val prefsTopicPure = IdMap.empty[TopicId, Score]

    /* Insert unassigned topics */
    fastLoop(0, slotsCount.value) { id => 
      topicsNames(id) = unassignedTopicName(slotsNames(id))
      topicsMin(id) = Count.Zero // TODO handle settings of people not being alone unassigned. Probably as a preference.
      topicsMax(id) = Count.maxCount[PersonId]
      topicsAllowedSlots(id) = SmallIdSet[SlotId](id)
      topicsForced = topicsForced + id
    }

    /* Then handle normal topics */
    var topicIdInt = slotsCount.value
    input.topics.foreach { (inTopic: InputTopic) =>
      val mandatories: SmallIdSet[PersonId] = SmallIdSet(input.persons.filter(_.mandatory.contains(inTopic.name)).map(_.name).map(personsIdByName)*)
      // TODO val forbidden : SmallIdSet[PersonId] = SmallIdSet(input.persons.filter(_.forbidden.contains(inTopic.name)).map(_.name).map(personsIdByName)*)
      val min: Count[PersonId] = inTopic.min.getOrElse(settings.defaultMinPersonsPerTopic)
      val max: Count[PersonId] = inTopic.max.getOrElse(settings.defaultMaxPersonsPerTopic)
      val allowedSlots: SmallIdSet[SlotId] = inTopic.slots.fold(SmallIdSet.full[SlotId]) { slots => SmallIdSet(slots.map(slotsIdByName).toSeq*) }

      // TODO adds exclusion for multi-occurrence
      // TODO multi-part topics: check on which slot each topic can happen (that leaves space for the followups)
      inTopic.occurrenceInstances.foreach { inTopicOcc =>
        /* for long topics, some stuff is only counted on the first part */
        inTopic.presence.foreach { score => prefsTopicPure(topicIdInt) = prefsTopicPure(topicIdInt) + score.value }
        inTopicOcc.partInstances.foreach { inTopicOccPart =>
          topicsNames(topicIdInt) = inTopicOccPart.name
          topicsMandatories(topicIdInt) = mandatories
          topicsMin(topicIdInt) = min
          topicsMax(topicIdInt) = max
          topicsAllowedSlots(topicIdInt) = allowedSlots
          if (inTopic.forced) {
            topicsForced = topicsForced + topicIdInt
          }
          topicsFollowup(topicIdInt) = topicIdInt + 1
          topicIdInt += 1
        }
        topicsFollowup(topicIdInt - 1) = TopicId.None // last part doesn't get a followup
      }
    }

  }



  /* Construction of the Problem */
  lazy val problem: SmallProblem = ???

  lazy val result: ValidatedNel[InputError, SmallProblem] = errors.toList.sorted.map(InputError(_)) match {
    case Nil => problem.valid
    case h :: q => NonEmptyList.of(h, q *).invalid[SmallProblem]
  }

}

object InputTranscription2 {
  /** A prefix on the topic-name for synthetic topics (topics created by Gaston, not present in the input). Currently
   * only unassigned topics, but reserving it just in case. */
  val TopicSyntheticPrefix = "@"

  private def unassignedTopicName(slotName: String): String = 
    s"${TopicSyntheticPrefix}Unassigned ($slotName)"

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
        .filter { t => t.name.contains(InputTopic.PartMarker) || t.name.contains(InputTopic.OccurrenceMarker) }
        .map { t => s"Topic [${t.name}]: Name cannot contain characters '${InputTopic.PartMarker}' or '${InputTopic.OccurrenceMarker}'" }
    } ++ {
      input.topics
        .filter { t => t.min.lazyZip(t.max).exists(_ > _) }
        .map { t => s"Topic [${t.name}]: Min (${t.min}) is higher than max (${t.max})" }
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

  def transcribe(input: InputModel): Either[InputErrors, SmallProblem] = new InputTranscription2(input).result.toEither
}
