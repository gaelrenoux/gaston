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
    val prefsTopicsExclusive: IdMap[PersonId, IdMatrixSymmetrical[TopicId, Score]], // reward (normally negative) for being on exclusive topics

    val prefsTopicsLinked: Array[SmallIdSet[TopicId]] // a person must be either on all linked topics, or on none of them
) {
  given CountAll[SlotId] = CountAll(slotsCount)
  given CountAll[TopicId] = CountAll(topicsCount)
  given CountAll[PersonId] = CountAll(personsCount)

  /** For each person, the set of topics for which they have an exclusive preference */
  val personsToTopicsWithPrefExclusive: IdMap[PersonId, SmallIdSet[TopicId]] =
    prefsTopicsExclusive.mapValues { matrix =>
      var result = SmallIdSet.empty[TopicId]
      topicsCount.foreach { tid1 =>
        topicsCount.foreach { tid2 =>
          if (matrix(tid1, tid2) != Score.Zero) {
            result += tid1
            result += tid2
          }
        }
      }
      result
    }

  val unassignedTopicsCount: Count[TopicId] = slotsCount.value
  val personTopicsMandatory: IdMap[PersonId, SmallIdSet[TopicId]] = {
    val personsToTopicMap = topicsMandatories.toSeq.flatMap { (tid, pids) =>
      pids.toSet.toSeq.map(_ -> tid)
    }.groupToMap
    IdMap.from(personsToTopicMap.mapValuesStrict(SmallIdSet(_*)))
  }

  def score(schedule: Schedule): Score = {
    val personalScores: Array[Score] = scorePersons(schedule).destructiveSortedValues
    val personalScoresTotal = personalScores.fastFoldRight(Score.Zero) { (score, acc) =>
      (SmallProblem.RankFactor * acc: Score) + score
    }
    val topicsPureTotal = schedule.topicsPresent.mapSumToScore(prefsTopicPure(_))
    personalScoresTotal + topicsPureTotal
  }

  /** Scores on personsBaseScore, prefsPersonTopic, prefsPersonPerson, prefsTopicsExclusive */
  // TODO inline this maybe ?
  def scorePersons(schedule: Schedule): IdMap[PersonId, Score] = {
    // TODO to ultra-optimize this, we could have all person scores as a single array: first the base score, then person/topic, then person/person, etc.
    schedule.personsToTopics.mapToScore { (pid, topicIds) =>
      val baseScore = personsBaseScore(pid)
      val wishesScore = scoreWishes(schedule, pid, topicIds)
      val exclusiveScore = scoreExclusive(pid, topicIds)
      val linkedScore = scoreLinked(topicIds)
      baseScore + wishesScore + exclusiveScore + linkedScore
    }
  }

  private def scoreWishes(schedule: Schedule, pid: PersonId, topicIds: SmallIdSet[TopicId]) = {
    topicIds.mapSumToScore { tid =>
      val topicsScore = prefsPersonTopic(pid, tid)
      /* Person antipathy doesn't apply on unassigned topics */
      val otherPersonsScore: Score = if (tid.value < unassignedTopicsCount.value) 0 else {
        val otherPersons = schedule.topicsToPersons(tid) - pid
        otherPersons.mapSumToScore(prefsPersonPerson(pid, _))
      }
      topicsScore + otherPersonsScore
    }
  }

  private def scoreExclusive(pid: PersonId, topicIds: SmallIdSet[TopicId]) = {
    var exclusiveScore = Score.Zero
    (topicIds && personsToTopicsWithPrefExclusive(pid)).foreachPair { (tid1, tid2) =>
      exclusiveScore += prefsTopicsExclusive(pid)(tid1, tid2)
    }
    exclusiveScore
  }

  private def scoreLinked(topicIds: SmallIdSet[TopicId]) = {
    var linkedScore = Score.Zero
    prefsTopicsLinked.fastForeach { linked =>
      val result = linked && topicIds
      if (result.nonEmpty && result != linked) {
        linkedScore += Score.MinReward.value * result.size
      }
    }
    linkedScore
  }

  @testOnly
  def calculateBaseScores(schedule: Schedule): Array[Score] = {
    schedule.personsToTopics.mapToScore { (pid, topicIds) =>
      personsBaseScore(pid)
    }.destructiveSortedValues
  }

  @testOnly
  def calculateWishesTopicScores(schedule: Schedule): Array[Score] = {
    schedule.personsToTopics.mapToScore { (pid, topicIds) =>
      topicIds.mapSumToScore { tid =>
        prefsPersonTopic(pid, tid)
      }
    }.destructiveSortedValues
  }

  @testOnly
  def calculateWishesPersonScores(schedule: Schedule): Array[Score] = {
    schedule.personsToTopics.mapToScore { (pid, topicIds) =>
      topicIds.mapSumToScore { tid =>
        /* Person antipathy doesn't apply on unassigned topics */
        if (tid.value < unassignedTopicsCount.value) 0 else {
          val otherPersons = schedule.topicsToPersons(tid) - pid
          otherPersons.mapSumToScore(prefsPersonPerson(pid, _))
        }
      }
    }.destructiveSortedValues
  }

  @testOnly
  def calculateExclusiveScores(schedule: Schedule): Array[Score] = {
    schedule.personsToTopics.mapToScore { (pid, topicIds) =>
      scoreExclusive(pid, topicIds)
    }.destructiveSortedValues
  }

  @testOnly
  def calculateLinkedScores(schedule: Schedule): Array[Score] = {
    schedule.personsToTopics.mapToScore { (pid, topicIds) =>
      scoreLinked(topicIds)
    }.destructiveSortedValues
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

}

object SmallProblem {
  val RankFactor: Weight = 0.5 // TODO move this to an appropriate class, maybe a Constants object ?
}
