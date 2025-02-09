package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{fastFoldRight, fastForeach, testOnly}


/** The schedule for a single slot. */
// TODO for all of those, check if having an Array isn't better than having a SmallIdSet
class SlotSchedule(
    val slotId: SlotId,
    var topics: SmallIdSet[TopicId],
    val personsToTopic: IdMap[PersonId, TopicId], // Default value (for persons that arent't there) is TopicId.None
    val topicsToPersons: IdMap[TopicId, SmallIdSet[PersonId]]
)(using
    val countTopics: CountAll[TopicId],
    val countPersons: CountAll[PersonId]
) {

  var personsScoreCache = IdMap.fill[PersonId, Score](Score.Missing)

  def invalidateCacheFor(pid: PersonId)(using problem: SmallProblem): Unit = {
    personsScoreCache(pid) = Score.Missing
    val otherPersons = problem.personsTargetedToPersonsWithWish(pid)
    otherPersons.foreach {
      personsScoreCache(_) = Score.Missing
    }
  }

  /** Returns true if that person can be added to this topic, without moving anyone else. */
  def isAddableToTopic(problem: SmallProblem, pid: PersonId, tid: TopicId) = {
    inline def topicMax = problem.topicsMax(tid)

    inline def topicPersonsCount = topicsToPersons(tid).size
    // TODO should handle forbidden here
    topicPersonsCount < topicMax
  }

  /** Returns true if that person can be removed from that topic, without moving anyone else. */
  def isDroppableTopic(problem: SmallProblem, pid: PersonId, tid: TopicId) = {
    inline def topicMin = problem.topicsMin(tid)

    inline def topicPersonsCount = topicsToPersons(tid).size

    inline def personIsMandatory = problem.isPersonMandatory(pid, tid)

    !personIsMandatory && topicPersonsCount > topicMin
  }

  inline def move(pid: PersonId, tid1: TopicId, tid2: TopicId)(using SmallProblem): SlotSchedule = {
    // TODO Dev mode control: pid should be on tid1
    personsToTopic(pid) = tid2
    topicsToPersons(tid1) = topicsToPersons(tid1) - pid
    topicsToPersons(tid2) = topicsToPersons(tid2) + pid
    invalidateCacheFor(pid)
    this
  }

  def reverseMove(pid: PersonId, tid1: TopicId, tid2: TopicId)(using SmallProblem): SlotSchedule = move(pid, tid2, tid1)

  inline def addTopic(tid: TopicId): SlotSchedule = {
    topics = topics + tid
    this
  }

  inline def removeTopic(sid: SlotId, tid: TopicId): SlotSchedule = {
    topics = topics - tid
    this
  }

  def isValidFor(pb: SmallProblem): Boolean = {
    // TODO Will be helpful in tests
    ???
  }

  /* ALL SCORING METHODS */

  def scoreNotPersons(problem: SmallProblem): Score = {
    this.topics.mapSumToScore(problem.prefsTopicPure(_))
  }

  /** Scores on prefsPersonTopic, prefsPersonPerson */
  // TODO inline this maybe ?
  def scorePersons(problem: SmallProblem): IdMap[PersonId, Score] = {
    // TODO to ultra-optimize this, we could have all person scores as a single array: first person/topic, then person/person.
    this.personsToTopic.mapToScore { (pid, tid) =>
      val cached = personsScoreCache(pid)
      if (cached != Score.Missing) {
        cached
      } else {
        val score = scoreWishes(problem, pid, tid)
        personsScoreCache(pid) = score
        score
      }
    }
  }

  private def scoreWishes(problem: SmallProblem, pid: PersonId, tid: TopicId): Score = {
    val hasPersonWishes = problem.personsWithPersonWish.contains(pid)
    val topicsScore: Score = problem.prefsPersonTopic(pid, tid)
    val otherPersonsScore: Score =
      if (!problem.personsWithPersonWish.contains(pid)) Score.Zero
      else if (tid.value < problem.unassignedTopicsCount.value) Score.Zero // Person sym/antipathy doesn't apply on unassigned topics
      else (topicsToPersons(tid) - pid).mapSumToScore(problem.prefsPersonPerson(pid, _))
    topicsScore + otherPersonsScore
  }
}

object SlotSchedule {
  def from(slotId: SlotId, topics: SmallIdSet[TopicId], assignment: IdMap[PersonId, TopicId])(using countTopics: CountAll[TopicId], countPersons: CountAll[PersonId]): SlotSchedule = {
    SlotSchedule(
      slotId,
      topics,
      assignment,
      assignment.transpose
    )
  }
}
