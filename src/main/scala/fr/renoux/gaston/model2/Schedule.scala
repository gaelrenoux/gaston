package fr.renoux.gaston.model2

import fr.renoux.gaston.util.testOnly
import fr.renoux.gaston.util.fastForeach
import fr.renoux.gaston.util.fastFoldRight


// TODO for all of those, check if having an Array isn't better than having a SmallIdSet
class Schedule(
    val slotsToTopics: IdMap[SlotId, SmallIdSet[TopicId]],
    val topicsToSlot: IdMap[TopicId, SlotId],
    var topicsPresent: SmallIdSet[TopicId],
    val personsToTopics: IdMap[PersonId, SmallIdSet[TopicId]],
    val topicsToPersons: IdMap[TopicId, SmallIdSet[PersonId]]
)(using
    val countSlots: CountAll[SlotId],
    val countTopics: CountAll[TopicId],
    val countPersons: CountAll[PersonId]
) {

  def topicOf(sid: SlotId, pid: PersonId): TopicId = {
    val topicsFromSlot = slotsToTopics(sid)
    val topicsFromPerson = personsToTopics(pid)
    (topicsFromSlot && topicsFromPerson).head // should always exist
  }

   /** Returns true if that person can be added to this topic, without moving anyone else. */
  def isAddableTopic(problem: SmallProblem, pid: PersonId, tid: TopicId) = {
    inline def topicMax = problem.topicsMax(tid)
    inline def topicPersonsCount = topicsToPersons(tid).size
    // TODO should handle forbidden here
    topicPersonsCount < topicMax
  }

  /** Returns true if that person can be removed from that topic, without moving anyone else. */
  def isDroppableTopic(problem: SmallProblem, pid: PersonId, tid: TopicId) = {
      inline def topicMin = problem.topicsMin(tid)
      inline def topicPersonsCount = topicsToPersons(tid).size
      inline def personIsMandatory = problem.isPersonMandatory(pid, tid)
      !personIsMandatory && topicPersonsCount > topicMin
  }

  inline def move(pid: PersonId, tid1: TopicId, tid2: TopicId): Schedule = {
    // TODO Dev mode control: both topics should be on the same slot, pid should be on tid1
    personsToTopics(pid) = personsToTopics(pid) - tid1 + tid2
    topicsToPersons(tid1) = topicsToPersons(tid1) - pid
    topicsToPersons(tid2) = topicsToPersons(tid2) + pid
    this
  }

  def reverseMove(pid: PersonId, tid1: TopicId, tid2: TopicId): Schedule = move(pid, tid2, tid1)

  inline def addTopic(sid: SlotId, tid: TopicId): Schedule = {
    slotsToTopics(sid) = slotsToTopics(sid) + tid
    topicsToSlot(tid) = sid
    topicsPresent = topicsPresent + tid
    this
  }

  inline def removeTopic(sid: SlotId, tid: TopicId): Schedule = {
    slotsToTopics(sid) = slotsToTopics(sid) - tid
    topicsToSlot(tid) = SlotId.None
    topicsPresent = topicsPresent - tid
    this
  }

  def isValidFor(pb: SmallProblem): Boolean = {
    // TODO Will be helpful in tests
    ???
  }

  /* ALL SCORING METHODS */

  def score(problem: SmallProblem): Score = {
    val personalScores: Array[Score] = scorePersons(problem).destructiveSortedValues
    val personalScoresTotal = personalScores.fastFoldRight(Score.Zero) { (score, acc) =>
      (SmallProblem.RankFactor * acc: Score) + score
    }
    val topicsPureTotal = this.topicsPresent.mapSumToScore(problem.prefsTopicPure(_))
    personalScoresTotal + topicsPureTotal
  }

  /** Scores on personsBaseScore, prefsPersonTopic, prefsPersonPerson, prefsTopicsExclusive */
  // TODO inline this maybe ?
  def scorePersons(problem: SmallProblem): IdMap[PersonId, Score] = {
    // TODO to ultra-optimize this, we could have all person scores as a single array: first the base score, then person/topic, then person/person, etc.
    this.personsToTopics.mapToScore { (pid, topicIds) =>
      val baseScore = problem.personsBaseScore(pid)
      val wishesScore = scoreWishes(problem, pid, topicIds)
      val exclusiveScore = scoreExclusive(problem, pid, topicIds)
      val linkedScore = scoreLinked(problem, topicIds)
      baseScore + wishesScore + exclusiveScore + linkedScore
    }
  }

  private def scoreWishes(problem: SmallProblem, pid: PersonId, topicIds: SmallIdSet[TopicId]) = {
    val hasPersonWishes = problem.personsWithPersonWish.contains(pid)
    topicIds.mapSumToScore { tid =>
      val topicsScore = problem.prefsPersonTopic(pid, tid)
      val otherPersonsScore: Score = scorePersonWishes(problem, pid, tid, hasPersonWishes)
      topicsScore + otherPersonsScore
    }
  }

  private def scorePersonWishes(problem: SmallProblem, pid: PersonId, tid: TopicId, hasPersonWishes: Boolean): Score = {
    /* Person sym/antipathy doesn't apply on unassigned topics */
    if (!hasPersonWishes || tid.value < problem.unassignedTopicsCount.value) 0
    else {
      val otherPersons = this.topicsToPersons(tid) - pid
      otherPersons.mapSumToScore(problem.prefsPersonPerson(pid, _))
    }
  }

  private def scoreExclusive(problem: SmallProblem, pid: PersonId, topicIds: SmallIdSet[TopicId]) =
    problem.prefsTopicsExclusive(pid).evaluate(topicIds)

  private def scoreLinked(problem: SmallProblem, topicIds: SmallIdSet[TopicId]) = {
    var linkedScore = Score.Zero
    problem.prefsTopicsLinked.fastForeach { linked =>
      val result = linked && topicIds
      if (result.nonEmpty && result != linked) {
        linkedScore += Score.MinReward.value * result.size.value
      }
    }
    linkedScore
  }

  @testOnly
  def calculateBaseScores(problem: SmallProblem): Array[Score] = {
    this.personsToTopics.mapToScore { (pid, topicIds) =>
      problem.personsBaseScore(pid)
    }.destructiveSortedValues
  }

  @testOnly
  def calculateWishesTopicScores(problem: SmallProblem): Array[Score] = {
    this.personsToTopics.mapToScore { (pid, topicIds) =>
      topicIds.mapSumToScore { tid =>
        problem.prefsPersonTopic(pid, tid)
      }
    }.destructiveSortedValues
  }

  @testOnly
  def calculateWishesPersonScores(problem: SmallProblem): Array[Score] = {
    this.personsToTopics.mapToScore { (pid, topicIds) =>
      val hasPersonWishes = problem.personsWithPersonWish.contains(pid)
      topicIds.mapSumToScore { tid =>
        scorePersonWishes(problem, pid, tid, hasPersonWishes)
      }
    }.destructiveSortedValues
  }

  @testOnly
  def calculateExclusiveScores(problem: SmallProblem): Array[Score] = {
    this.personsToTopics.mapToScore { (pid, topicIds) =>
      scoreExclusive(problem, pid, topicIds)
    }.destructiveSortedValues
  }

  @testOnly
  def calculateLinkedScores(problem: SmallProblem): Array[Score] = {
    this.personsToTopics.mapToScore { (pid, topicIds) =>
      scoreLinked(problem, topicIds)
    }.destructiveSortedValues
  }
}

object Schedule {
  def from(
      planning: IdMap[SlotId, SmallIdSet[TopicId]],
      assignment: IdMap[PersonId, SmallIdSet[TopicId]]
  )(using
      countSlots: CountAll[SlotId],
      countTopics: CountAll[TopicId],
      countPersons: CountAll[PersonId]
  ) = {
    Schedule(
      planning,
      planning.transpose.mapValues(_.headOrElse(SlotId.None)),
      planning.reduceValues(_ ++ _),
      assignment,
      assignment.transpose
    )
  }
}
