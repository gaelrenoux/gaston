package fr.renoux.gaston.model2

import fr.renoux.gaston.util.CanGroupToMap.given
import fr.renoux.gaston.util.{Count as _, *}


/** Description on a problem small enough that we can use SmallIdSets (so max 64 elements of each category). */
final class SmallProblem(
    val slotsCount: Count[SlotId],
    val slotsToName: IdMap[SlotId, String],
    val slotsToPersonsPresent: IdMap[SlotId, SmallIdSet[PersonId]],
    val slotsToNextSlot: IdMap[SlotId, SlotId],
    val slotsToMaxTopics: IdMap[SlotId, Count[TopicId]],

    val topicsCount: Count[TopicId],
    val topicsToName: IdMap[TopicId, String], // Includes unassigned topics
    val topicsToMandatories: IdMap[TopicId, SmallIdSet[PersonId]],
    val topicsToForbiddens: IdMap[TopicId, SmallIdSet[PersonId]],

    val topicsToMinPersons: IdMap[TopicId, Count[PersonId]],
    val topicsToMaxPersons: IdMap[TopicId, Count[PersonId]],
    val topicsToAllowedSlots: IdMap[TopicId, SmallIdSet[SlotId]],
    val topicsToFollowup: IdMap[TopicId, TopicId],
    val topicsForced: SmallIdSet[TopicId],
    val topicsToSimultaneous: IdMap[TopicId, SmallIdSet[TopicId]],
    val topicsToNotSimultaneous: IdMap[TopicId, SmallIdSet[TopicId]],

    val personsCount: Count[PersonId],
    val personsToName: IdMap[PersonId, String],
    val personsToWeight: IdMap[PersonId, Weight], // Unused in calculation as weight has already been applied to all relevant scores. Only for display.
    val personsToBaseScore: IdMap[PersonId, Score],

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
  val personsToMandatoryTopics: IdMap[PersonId, SmallIdSet[TopicId]] = {
    val personsToTopicMap = topicsToMandatories.toSeq.flatMap { (tid, pids) =>
      pids.toSet.toSeq.map(_ -> tid)
    }.groupToMap
    IdMap.from(personsToTopicMap.mapValuesStrict(SmallIdSet(_ *)))
  }

  val personsWithPersonWish: SmallIdSet[PersonId] = SmallIdSet(
    prefsPersonPerson.toMap2.collect {
      case (i, line) if line.exists(_._2 != Score.Zero) => i
    }
  )

  /** This is a reverse from prefsPersonPerson: it shows for each person, which one has targeted them with a wish. */
  val personsTargetedToPersonsWithWish: IdMap[PersonId, SmallIdSet[PersonId]] = {
    val targetSourcePids = prefsPersonPerson.toMap2.toSeq.flatMap { (sourcePid, targetPidsToScore) =>
      targetPidsToScore.filter(_._2 != Score.Zero).keySet.map(_ -> sourcePid)
    }
    val targetToSourcePids = targetSourcePids.groupToMap
    IdMap.from(targetToSourcePids.view.mapValues(SmallIdSet(_)))
  }

  def copy() = new SmallProblem(
    slotsCount = slotsCount,
    slotsToName = slotsToName.copy(),
    slotsToPersonsPresent = slotsToPersonsPresent.copy(),
    slotsToNextSlot = slotsToNextSlot.copy(),
    slotsToMaxTopics = slotsToMaxTopics.copy(),
    topicsCount = topicsCount,
    topicsToName = topicsToName.copy(),
    topicsToMandatories = topicsToMandatories.copy(),
    topicsToForbiddens = topicsToForbiddens.copy(),
    topicsToMinPersons = topicsToMinPersons.copy(),
    topicsToMaxPersons = topicsToMaxPersons.copy(),
    topicsToAllowedSlots = topicsToAllowedSlots.copy(),
    topicsToFollowup = topicsToFollowup.copy(),
    topicsForced = topicsForced,
    topicsToSimultaneous = topicsToSimultaneous.copy(),
    topicsToNotSimultaneous = topicsToNotSimultaneous.copy(),
    personsCount = personsCount,
    personsToName = personsToName.copy(),
    personsToWeight = personsToWeight.copy(),
    personsToBaseScore = personsToBaseScore.copy(),
    prefsPersonTopic = prefsPersonTopic.copy(),
    prefsPersonPerson = prefsPersonPerson.copy(),
    prefsTopicPure = prefsTopicPure.copy(),
    prefsTopicsExclusive = prefsTopicsExclusive.copy(),
    prefsTopicsLinked = prefsTopicsLinked.fastCopy()
  )

  /** Returns true if this is an "unassigned" topic. */
  inline def isTopicUnassigned(tid: TopicId): Boolean = tid.value < unassignedTopicsCount.value

  inline def isPersonMandatory(pid: PersonId, tid: TopicId): Boolean = topicsToMandatories(tid).contains(pid)

  inline def isPersonForbidden(pid: PersonId, tid: TopicId): Boolean = topicsToForbiddens(tid).contains(pid)

  @testOnly
  def getTopicIdByName(name: String): TopicId = topicsToName.valuesSeq.indexOf(name)

  extension (pid: PersonId) {
    def personName: String = personsToName(pid)
  }
  extension (tid: TopicId) {
    def topicName: String = topicsToName(tid)
  }
  extension (sid: SlotId) {
    def slotName: String = slotsToName(sid)
  }

  override def toString: String = s"SmallProblem($slotsCount/$topicsCount/$personsCount)"

}

object SmallProblem {
  val RankFactor: Weight = 0.5 // TODO move this to an appropriate class, maybe a Constants object ?
}
