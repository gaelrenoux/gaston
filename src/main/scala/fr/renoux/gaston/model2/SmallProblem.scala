package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{Count as _, *}
import fr.renoux.gaston.util.CanGroupToMap.given


/** Description on a problem small enough that we can use SmallIdSets (so max 64 elements of each category). */
final class SmallProblem(
    val slotsCount: Count[SlotId],
    val slotsNames: IdMap[SlotId, String],
    val slotsPersonsPresent: IdMap[SlotId, SmallIdSet[PersonId]],
    val slotsToNextSlot: IdMap[SlotId, SlotId],
    val slotsMaxTopics: IdMap[SlotId, Count[TopicId]],

    val topicsCount: Count[TopicId],
    val topicsName: IdMap[TopicId, String], // Includes unassigned topics
    val topicsMandatories: IdMap[TopicId, SmallIdSet[PersonId]],
    val topicsMin: IdMap[TopicId, Count[PersonId]],
    val topicsMax: IdMap[TopicId, Count[PersonId]],
    val topicsAllowedSlots: IdMap[TopicId, SmallIdSet[SlotId]],
    val topicsFollowup: IdMap[TopicId, TopicId],
    val topicsForced: SmallIdSet[TopicId],
    val topicsSimultaneous: IdMap[TopicId, SmallIdSet[TopicId]],
    val topicsNotSimultaneous: IdMap[TopicId, SmallIdSet[TopicId]],

    val personsCount: Count[PersonId],
    val personsName: IdMap[PersonId, String],
    val personsWeight: IdMap[PersonId, Weight], // Unused in calculation as weight has already been applied to all relevant scores. Only for display.
    val personsBaseScore: IdMap[PersonId, Score],

    val prefsPersonTopic: IdMatrix[PersonId, TopicId, Score], // also includes forbidden topics
    val prefsPersonPerson: IdMatrix[PersonId, PersonId, Score],
    val prefsTopicPure: IdMap[TopicId, Score], // score added for simply having this topic on schedule
    val prefsTopicsExclusive: IdMap[PersonId, Exclusivities],

    val prefsTopicsLinked: Array[SmallIdSet[TopicId]] // a person must be either on all linked topics, or on none of them
) {
  given CountAll[SlotId] = CountAll(slotsCount)
  given CountAll[TopicId] = CountAll(topicsCount)
  given CountAll[PersonId] = CountAll(personsCount)

  val unassignedTopicsCount: Count[TopicId] = slotsCount.value
  val personTopicsMandatory: IdMap[PersonId, SmallIdSet[TopicId]] = {
    val personsToTopicMap = topicsMandatories.toSeq.flatMap { (tid, pids) =>
      pids.toSet.toSeq.map(_ -> tid)
    }.groupToMap
    IdMap.from(personsToTopicMap.mapValuesStrict(SmallIdSet(_*)))
  }

  val personsWithPersonWish: SmallIdSet[PersonId] = SmallIdSet(
    prefsPersonPerson.toMap2.collect {
      case (i, line) if line.exists(_._2 != Score.Zero) => i
    }
  )

  /** This is a reverse from prefsPersonPerson: it shows for each person, which one has targeted them with a wish. */
  val personsTargetedByWish: IdMap[PersonId, SmallIdSet[PersonId]] = {
    val targetSourcePids = prefsPersonPerson.toMap2.toSeq.flatMap { (sourcePid, targetPidsToScore) =>
      targetPidsToScore.filter(_._2 != Score.Zero).keySet.map(_ -> sourcePid)
    }
    val targetToSourcePids = targetSourcePids.groupToMap
    IdMap.from(targetToSourcePids.view.mapValues(SmallIdSet(_)))
  }

  def copy() = new SmallProblem(
    slotsCount = slotsCount,
    slotsNames = slotsNames.copy(),
    slotsPersonsPresent = slotsPersonsPresent.copy(),
    slotsToNextSlot = slotsToNextSlot.copy(),
    slotsMaxTopics = slotsMaxTopics.copy(),
    topicsCount = topicsCount,
    topicsName = topicsName.copy(),
    topicsMandatories = topicsMandatories.copy(),
    topicsMin = topicsMin.copy(),
    topicsMax = topicsMax.copy(),
    topicsAllowedSlots = topicsAllowedSlots.copy(),
    topicsFollowup = topicsFollowup.copy(),
    topicsForced = topicsForced,
    topicsSimultaneous = topicsSimultaneous.copy(),
    topicsNotSimultaneous = topicsNotSimultaneous.copy(),
    personsCount = personsCount,
    personsName = personsName.copy(),
    personsWeight = personsWeight.copy(),
    personsBaseScore = personsBaseScore.copy(),
    prefsPersonTopic = prefsPersonTopic.copy(),
    prefsPersonPerson = prefsPersonPerson.copy(),
    prefsTopicPure = prefsTopicPure.copy(),
    prefsTopicsExclusive = prefsTopicsExclusive.copy(),
    prefsTopicsLinked = prefsTopicsLinked.fastCopy()
  )

  inline def isPersonMandatory(pid: PersonId, tid: TopicId) = topicsMandatories(tid).contains(pid)

}

object SmallProblem {
  val RankFactor: Weight = 0.5 // TODO move this to an appropriate class, maybe a Constants object ?
}
