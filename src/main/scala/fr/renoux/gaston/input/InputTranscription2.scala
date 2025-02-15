package fr.renoux.gaston.input

import cats.data.NonEmptyList
import cats.data.ValidatedNel
import cats.implicits.*
import fr.renoux.gaston.model.{Score as _, Weight as _, *}
import fr.renoux.gaston.model2.*
import fr.renoux.gaston.util.{Count as _, *}
import fr.renoux.gaston.util.CanGroupToMap.given
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.{Constraint as _, *}
import scala.collection.mutable


/** Converts the Input object (canonical input) to the Problem object (internal representation of the problem to
 * optimize).
 *
 * It is split in various fields for clarity and ease of development. To use it, simply instantiate it and get the end
 * result in the `result` field.
 */
final class InputTranscription2(rawInput: InputModel) {

  import fr.renoux.gaston.input.InputTranscription2.*

  // val input: InputModel = rawInput.clean // TODO Restore cleaner here
  val input: InputModel = rawInput
  val settings: InputSettings = input.settings

  /* Checking errors */
  lazy val errors: Set[String] = checkErrors(input)



  /* Get all counts first */
  given slotsCount: CountAll[SlotId] = CountAll[SlotId](input.slots.flatten.size)
  given topicsCount: CountAll[TopicId] = CountAll[TopicId](slotsCount.value + input.topics.map { inT => inT.forcedOccurrences * inT.forcedDuration }.sum)
  given personsCount: CountAll[PersonId] = CountAll[PersonId](input.persons.size)

  val unassignedTopicsCount: Count[TopicId] = slotsCount.value
  val unassignedTopicsSet: SmallIdSet[TopicId] = SmallIdSet(unassignedTopicsCount.range*)


  /* Settings */
  lazy val settingsUnassignedPrefByPerson: IdMap[PersonId, Score] = {
    if (!input.settings.unassigned.allowed) IdMap.fill[PersonId, Score](Score.MinReward)
    else input.settings.unassigned.personAntiPreferenceScaling match {
      case None => IdMap.fill[PersonId, Score](input.settings.unassigned.personAntiPreference.value)
      case Some(scaling) => IdMap.tabulate[PersonId, Score] { pid =>
        val forbiddenCount = input.persons(pid.value).forbidden.size
        val forbiddenRatio = forbiddenCount.toDouble / input.topics.size
        val scoreRatio = math.min(1, forbiddenRatio / scaling.forbiddenRatioForMaximum)
        val variableScore: Score = scaling.maximumAntiPreference.value - input.settings.unassigned.personAntiPreference.value
        input.settings.unassigned.personAntiPreference.value + (variableScore * scoreRatio)
      }
    }
  }
  // TODO No scaling for the exclusive prefs on unassigned topics, should it be the case? It's added to the unassigned score which is already scaling, so maybe not.



  /* Persons */
  lazy val personsName: IdMap[PersonId, String] = IdMap.unsafeFrom[PersonId, String](input.persons.map(_.name: String).toArray)
  lazy val personsWeight: IdMap[PersonId, Weight] = IdMap.unsafeFrom[PersonId, Weight](input.persons.map(_.weight.value: Weight).toArray)
  lazy val personsBaseScore: IdMap[PersonId,Score] = IdMap.unsafeFrom[PersonId, Score](input.persons.map(p => p.baseScore.value / p.weight.value: Score).toArray)
  lazy val personsIdByName: Map[String, PersonId] = personsName.toReverseMap



  
  /* Slots */
  private lazy val flattenedSlots = input.slots.flatten
  lazy val slotsNames: IdMap[SlotId, String] = IdMap.unsafeFrom[SlotId, String](flattenedSlots.map(_.name: String).toArray)
  lazy val slotsMaxTopics: IdMap[SlotId, Count[TopicId]] = IdMap.unsafeFrom[SlotId, Count[TopicId]] {
    flattenedSlots.map(s => s.maxTopics.orElse(settings.defaultMaxTopicsPerSlot).getOrElse(Count.maxCount[TopicId])).toArray
  }
  lazy val slotsToNextSlot: IdMap[SlotId, SlotId] = {
    val result = IdMap.tabulate[SlotId, SlotId](_.value + 1)
    var index = -1
    input.slots.map(_.size).foreach { seqLen =>
      index += seqLen
      result(index) = SlotId.None
    }
    result
  }
  lazy val slotsIdByName: Map[String, SlotId] = slotsNames.toReverseMap
  private lazy val absencesBySlotId = input.persons.zipWithIndex.flatMap { (p, pid: PersonId) => p.absences.map(a => slotsIdByName(a) -> pid) }.groupToMap
  lazy val slotsPersonsPresent: IdMap[SlotId, SmallIdSet[PersonId]] =
    IdMap.tabulate[SlotId, SmallIdSet[PersonId]] { slotId => SmallIdSet.full[PersonId] -- absencesBySlotId.getOrElse(slotId, Nil) }



  /* Topics */
  lazy object topics {
    val topicsName = IdMap.empty[TopicId, String]
    val topicsMandatories = IdMap.fill[TopicId, SmallIdSet[PersonId]](SmallIdSet.empty[PersonId])
    val topicsMin = IdMap.empty[TopicId, Count[PersonId]]
    val topicsMax = IdMap.empty[TopicId, Count[PersonId]]
    val topicsAllowedSlots = IdMap.fill[TopicId, SmallIdSet[SlotId]](SmallIdSet.full[SlotId])
    var topicsForced = SmallIdSet.empty[TopicId]
    val topicsFollowup = IdMap.fill[TopicId, TopicId](TopicId.None)
    val topicsIdsByBaseName = mutable.Map[String, mutable.Set[TopicId]]()
    val topicsFirstPartIdsByBaseName = mutable.Map[String, mutable.Set[TopicId]]()

    /* Insert unassigned topics */
    fastLoop(0, slotsCount.value) { id => 
      topicsName(id) = unassignedTopicName(slotsNames(id))
      topicsMin(id) = Count.Zero // TODO handle settings of people not being alone unassigned. Probably as a preference.
      topicsMax(id) = Count.maxCount[PersonId]
      topicsAllowedSlots(id) = SmallIdSet[SlotId](id)
      topicsForced = topicsForced + id
      topicsIdsByBaseName(topicsName(id)) = mutable.Set(id)
      topicsFirstPartIdsByBaseName(topicsName(id)) = mutable.Set(id)
    }

    /* Then handle normal topics */
    var topicIdInt = slotsCount.value
    input.topics.foreach { (inTopic: InputTopic) =>
      val mandatories: SmallIdSet[PersonId] = SmallIdSet(input.persons.filter(_.mandatory.contains(inTopic.name)).map(_.name).map(personsIdByName)*)
      val min: Count[PersonId] = inTopic.min.getOrElse(settings.defaultMinPersonsPerTopic)
      val max: Count[PersonId] = inTopic.max.getOrElse(settings.defaultMaxPersonsPerTopic)
      val allowedSlots: SmallIdSet[SlotId] = inTopic.slots.fold(SmallIdSet.full[SlotId]) { slots => SmallIdSet(slots.map(slotsIdByName).toSeq*) }
      topicsIdsByBaseName(inTopic.name) = mutable.Set()
      topicsFirstPartIdsByBaseName(inTopic.name) = mutable.Set()

      // TODO multi-part topics: check on which slot each topic can happen (that leaves space for the followups)
      inTopic.occurrenceInstances.foreach { inTopicOcc =>
        topicsFirstPartIdsByBaseName(inTopic.name) += topicIdInt
        inTopicOcc.partInstances.foreach { inTopicOccPart =>
          topicsName(topicIdInt) = inTopicOccPart.name
          topicsMandatories(topicIdInt) = mandatories
          topicsMin(topicIdInt) = min
          topicsMax(topicIdInt) = max
          topicsAllowedSlots(topicIdInt) = allowedSlots
          if (inTopic.forced) {
            topicsForced = topicsForced + topicIdInt
          }
          topicsFollowup(topicIdInt) = topicIdInt + 1
          topicsIdsByBaseName(inTopic.name) += topicIdInt
          topicIdInt += 1
        }
        topicsFollowup(topicIdInt - 1) = TopicId.None // last part doesn't get a followup
      }
    }

    val topicsIdByName: Map[String, TopicId] = topicsName.toReverseMap
  }



  /* Other constraints */
  lazy object constraints {

    val topicsSimultaneous: IdMap[TopicId, SmallIdSet[TopicId]] = IdMap.fill[TopicId, SmallIdSet[TopicId]](SmallIdSet.empty[TopicId])
    val topicsNotSimultaneous: IdMap[TopicId, SmallIdSet[TopicId]] = IdMap.fill[TopicId, SmallIdSet[TopicId]](SmallIdSet.empty[TopicId])
    input.constraints.simultaneous.fastForeach { inConstraint =>
      val topicIds = inConstraint.topics.flatMap(topics.topicsIdsByBaseName) // TODO Maybe just look at the first parts only
      topicIds.fastForeach { tid =>
        topicsSimultaneous(tid) = topicsSimultaneous(tid) ++ topicIds - tid
      }
    }
    input.constraints.notSimultaneous.fastForeach { inConstraint =>
      val topicIds = inConstraint.topics.flatMap(topics.topicsIdsByBaseName) // TODO Maybe just look at the first parts only
      topicIds.fastForeach { tid =>
        topicsNotSimultaneous(tid) = topicsNotSimultaneous(tid) ++ topicIds - tid
      }
    }
    // TODO Complete not simultaneous with the ones which have conflicting mandatories

  }



  /* Preferences */
  lazy object preferences {

    /** Wishes are scaled so that everyone has the same maximum score. Otherwise, you could put either very small scores
     * (and therefore stay the lowest score in the schedule and therefore privileged when improving), or with such
     * high values that everyone else's preferences don't matter anymore. */
    // TODO Right now, negative prefs are ignored in the total count. Either handle them or just forbid negative wishes. Easy handling could be to add whatever is necessary to all make them positive.
    // TODO a wish on a multi-duration topic should count as many times as the duration, to avoid penalizing that person heavily (reward will be gained on each part)
    // TODO person wishes should count more than once, because they can be satisfied more than once
    lazy val personsScoreFactors: Map[PersonId, Weight] = input.personsSet.view.map { inPerson =>
      val pid = personsIdByName(inPerson.name)
      val totalTopicWishesScore = inPerson.wishes.filter(_._2.value > 0).values.sum.value
      val totalPersonWishesScore = inPerson.personWishes.filter(_._2.value > 0).values.sum.value
      val totalWishScore = totalTopicWishesScore + totalPersonWishesScore
      val scoreFactor = Constants.PersonTotalScore.value / totalWishScore
      pid -> (scoreFactor / inPerson.weight.value) // also add the person's weight in there
    }.toMap

    val prefsPersonTopic: IdMatrix[PersonId, TopicId, Score] = IdMatrix.fill[PersonId, TopicId, Score](Score.Zero)
    val prefsPersonPerson: IdMatrix[PersonId, PersonId, Score] = IdMatrix.fill[PersonId, PersonId, Score](Score.Zero)
    input.persons.foreach { inPerson =>
      val pid = personsIdByName(inPerson.name)
      val factor = personsScoreFactors(pid)
      inPerson.wishes.foreach { (topicName, score) =>
        // TODO Long-duration topics are counted twice as a reward. Is that correct ? Maybe put a half-score on te second part ?
        topics.topicsIdsByBaseName(topicName).foreach { tid =>
          prefsPersonTopic(pid, tid) = prefsPersonTopic(pid, tid) + (score.value * factor.value)
        }
      }
      inPerson.personWishes.foreach { (personName, score) =>
        val pid2 = personsIdByName(personName)
        prefsPersonPerson(pid, pid2) = prefsPersonPerson(pid, pid2) + (score.value * factor.value)
      }
      inPerson.forbidden.foreach { topicName =>
        topics.topicsIdsByBaseName(topicName).foreach { tid =>
          prefsPersonTopic(pid, tid) = Score.MinReward
        }
      }
      inPerson.incompatible.foreach { personName =>
        val pid2 = personsIdByName(personName)
        // Needs to take the weight, because the whole score of the person (total of all preferences) needs the weight applied
        prefsPersonPerson(pid, pid2) = input.settings.incompatibilityAntiPreference.value / inPerson.weight.value
      }
      unassignedTopicsCount.foreach { tid =>
        // Needs to take the weight, because the whole score of the person (total of all preferences) needs the weight applied
        prefsPersonTopic(pid, tid) = settingsUnassignedPrefByPerson(pid).value / inPerson.weight.value
      }
    }

    // TODO Handle inputPerson.minFreeSlots

    val prefsTopicPure = IdMap.empty[TopicId, Score]
    input.topics.foreach { (inTopic: InputTopic) =>
      inTopic.occurrenceInstances.foreach { inTopicOcc =>
        /* for long topics, topic presence score is only counted on the first part */
        val firstPart = inTopicOcc.partInstances.head
        val topicId = topics.topicsIdByName(firstPart.name)
        inTopic.presence.foreach { score => prefsTopicPure(topicId) = score.value }
      }
    }

    val prefsTopicsExclusiveBuffer = mutable.Map[PersonId, (mutable.Buffer[SmallIdSet[TopicId]], mutable.Buffer[Score])]()
    personsCount.foreach { pid =>
      prefsTopicsExclusiveBuffer(pid) = (mutable.Buffer(), mutable.Buffer())
    }
    /* Exclusive preference over unassigned topics */
    if (input.settings.unassigned.allowed) input.settings.unassigned.personMultipleAntiPreference.foreach { score =>
      personsCount.foreach { pid =>
        val inPerson = input.persons(pid.value)
        val bufferEntry =  prefsTopicsExclusiveBuffer(pid)
        bufferEntry._1.append(unassignedTopicsSet)
        bufferEntry._2.append(score.value / inPerson.weight.value)
      }
    }
    /* Exclusive preference over multi-occurrence topics */
    input.topics.filter(_.forcedOccurrences > 1).foreach { (inTopic: InputTopic) =>
      val occurrenceFirstParts = inTopic.occurrenceInstances.map(_.partInstances.head)
      val occurrenceFirstPartsIds = occurrenceFirstParts.map(t => topics.topicsIdByName(t.name))
      val mandatoriesIds = topics.topicsMandatories(occurrenceFirstPartsIds.head)
      val topicIdSet = SmallIdSet(occurrenceFirstPartsIds*)
      personsCount.foreach { pid =>
        if (!mandatoriesIds.contains(pid)) {
          val bufferEntry = prefsTopicsExclusiveBuffer(pid)
          bufferEntry._1.append(topicIdSet)
          bufferEntry._2.append(Score.MinReward)
        }
      }
    }
    /* Exclusive preference explicitly required by global constraints */
    input.constraints.exclusive.foreach { inConstraint =>
      val exemptedPersonIds = inConstraint.exemptions.map(personsIdByName)
      inConstraint.topics.develop(topics.topicsFirstPartIdsByBaseName(_).toSet).foreach { topicIds =>
        personsCount.foreach { pid =>
          if (!exemptedPersonIds.contains(pid)) {
            val bufferEntry = prefsTopicsExclusiveBuffer(pid)
            bufferEntry._1.append(SmallIdSet(topicIds))
            bufferEntry._2.append(Score.MinReward)
          }
        }
      }
    }
    val prefsTopicsExclusive: IdMap[PersonId, Exclusivities] = IdMap.from[PersonId, Exclusivities](
      prefsTopicsExclusiveBuffer.view.mapValues { case (topicSets, scores) =>
        Exclusivities(topicSets.toArray, scores.toArray)
      }
    )

    val prefsTopicsLinked: Array[SmallIdSet[TopicId]] = {
      val linkedFromConstraints = input.constraints.linked.map { inConstraint =>
        /* No multi-occurrence topic in global constraints, and we only need to link the first part of each topic */
        val topicIds = inConstraint.topics.map { baseTopicName => topics.topicsIdsByBaseName(baseTopicName).min }
        SmallIdSet(topicIds)
      }
      val linkedFromDuration = input.topics.filter(_.forcedDuration > 1).flatMap { inTopic =>
        /* Link separately the parts of each occurrence */
        inTopic.occurrenceInstances.map { inTopicOcc =>
          val topicIds = inTopicOcc.partInstances.map(_.name).map(topics.topicsIdByName)
          SmallIdSet(topicIds*)
        }
      }
      linkedFromConstraints.toArray ++ linkedFromDuration.toArray
    }

  } // end preferences



  /* Construction of the Problem */
  lazy val problem: SmallProblem = SmallProblem(
    slotsCount = slotsCount,
    slotsNames = slotsNames,
    slotsPersonsPresent = slotsPersonsPresent,
    slotsToNextSlot = slotsToNextSlot,
    slotsMaxTopics = slotsMaxTopics,
    topicsCount = topicsCount,
    topicsName = topics.topicsName,
    topicsMandatories = topics.topicsMandatories,
    topicsMin = topics.topicsMin,
    topicsMax = topics.topicsMax,
    topicsAllowedSlots = topics.topicsAllowedSlots,
    topicsFollowup = topics.topicsFollowup,
    topicsForced = topics.topicsForced,
    topicsSimultaneous = constraints.topicsSimultaneous,
    topicsNotSimultaneous = constraints.topicsNotSimultaneous,
    personsCount = personsCount,
    personsName = personsName,
    personsWeight = personsWeight,
    personsBaseScore = personsBaseScore,
    prefsPersonTopic = preferences.prefsPersonTopic,
    prefsPersonPerson = preferences.prefsPersonPerson,
    prefsTopicPure = preferences.prefsTopicPure,
    prefsTopicsExclusive = preferences.prefsTopicsExclusive,
    prefsTopicsLinked = preferences.prefsTopicsLinked,
  )

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
    } ++ {
      val empty = input.slots.zipWithIndex.filter(_._1.isEmpty)
      empty.map { (_, i) => s"Empty slot sequence at position $i" }
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
      input.constraints.exclusive
        .filter(_.topics.size < 2)
        .map(c => s"Exclusive constraint: should contain at least two topics: [${c.topics}]")
    } ++ {
      input.constraints.linked
        .flatMap(_.topics)
        .filter(!input.topicsNameSet.contains(_))
        .map(t => s"Linked constraint: unknown topic: [$t]")
    } ++ {
      input.constraints.linked
        .flatMap(_.topics)
        .flatMap(input.topicsByName.get)
        .filter(_.forcedOccurrences > 1)
        .map(t => s"Linked constraint: can't handle multi-occurrence topics: [$t]")
    } ++ {
      input.constraints.linked
        .filter(_.topics.size < 2)
        .map(c => s"Linked constraint: should contain at least two topics: [${c.topics}]")
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
            Some(s"Simultaneous constraint: different occurrence count: [${inConstraint.topics.mkString(", ")}]") // TODO Not sure about that one
          } else None
        }
    } ++ {
      input.constraints.simultaneous
        .flatMap { inConstraint =>
          val topics = inConstraint.topics.flatMap(input.topicsByName.get)
          topics.filter(_.forcedDuration > 1).map { topic =>
            s"Simultaneous constraint: can't handle long-duration topic: [${topic.name}]"
          }
        }
    } ++ {
      input.constraints.simultaneous
        .filter(_.topics.size < 2)
        .map(c => s"Simultaneous constraint: should contain at least two topics: [${c.topics}]")
    } ++ {
      input.constraints.notSimultaneous
        .flatMap(_.topics)
        .filter(!input.topicsNameSet.contains(_))
        .map(t => s"Not-simultaneous constraint: unknown topic: [$t]")
    } ++ {
      input.constraints.notSimultaneous
        .filter(_.topics.size < 2)
        .map(c => s"SNot-simultaneous constraint: should contain at least two topics: [${c.topics}]")
    }
  }

  def transcribe(input: InputModel): Either[InputErrors, SmallProblem] = new InputTranscription2(input).result.toEither
}
