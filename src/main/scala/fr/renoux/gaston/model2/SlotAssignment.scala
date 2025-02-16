package fr.renoux.gaston.model2

import scala.compiletime.uninitialized
import fr.renoux.gaston.util.*


/** The schedule for a single slot. */
class SlotAssignment(
    val problem: SmallProblem,
    val slot: SlotId,
    val personsToTopic: IdMap[PersonId, TopicId], // Default value (for persons that arent't there) is TopicId.None
) {

  import problem.given

  var parent: Schedule = uninitialized // Will be set up on initialization of the schedule

  // TODO for all of those, check if having an Array isn't better than having a SmallIdSet
  val topicsToPersons: IdMap[TopicId, SmallIdSet[PersonId]] = personsToTopic.transpose

  /** Returns true if that person can be added to this topic, without moving anyone else. */
  def isAddableToTopic(pid: PersonId, tid: TopicId): Boolean = {
    inline def topicMax = problem.topicsMax(tid)

    inline def topicPersonsCount = topicsToPersons(tid).size
    // TODO should handle forbidden here
    topicPersonsCount < topicMax
  }

  /** Returns true if that person can be removed from that topic, without moving anyone else. */
  def isDroppableFromTopic(pid: PersonId, tid: TopicId): Boolean = {
    inline def topicMin = problem.topicsMin(tid)

    inline def topicPersonsCount = topicsToPersons(tid).size

    inline def personIsMandatory = problem.isPersonMandatory(pid, tid)

    !personIsMandatory && topicPersonsCount > topicMin
  }

  def move(pid: PersonId, tid1: TopicId, tid2: TopicId): SlotAssignment = {
    // TODO Dev mode control: pid should be on tid1
    parent.saveCache()
    _move(pid, tid1, tid2)
    this
  }

  def undoMove(pid: PersonId, tid1: TopicId, tid2: TopicId): SlotAssignment = {
    _move(pid, tid2, tid1)
    parent.restoreCache()
    this
  }

  private inline def _move(pid: PersonId, tid1: TopicId, tid2: TopicId): Unit = {
    // TODO Dev mode control: pid should be on tid1
    personsToTopic(pid) = tid2
    topicsToPersons(tid1) = topicsToPersons(tid1) - pid
    topicsToPersons(tid2) = topicsToPersons(tid2) + pid
    parent.personsToTopics(pid) = parent.personsToTopics(pid) - tid1 + tid2 // TODO Not a fan of this
    invalidateCacheForPersons(pid)
  }

  def swap(pid1: PersonId, tid1: TopicId, pid2: PersonId, tid2: TopicId): SlotAssignment = {
    // TODO Dev mode control: pid1 should be on tid1, pid2 should be on tid2
    parent.saveCache()
    _swap(pid1, tid1, pid2, tid2)
    this
  }

  def undoSwap(pid1: PersonId, tid1: TopicId, pid2: PersonId, tid2: TopicId): SlotAssignment = {
    _swap(pid1, tid2, pid2, tid1)
    parent.restoreCache()
    this
  }

  private inline def _swap(pid1: PersonId, tid1: TopicId, pid2: PersonId, tid2: TopicId): Unit = {
    // TODO Dev mode control: pid1 should be on tid1, pid2 should be on tid2
    personsToTopic(pid1) = tid2
    personsToTopic(pid2) = tid1
    topicsToPersons(tid1) = topicsToPersons(tid1) - pid1 + pid2
    topicsToPersons(tid2) = topicsToPersons(tid2) + pid1 - pid2
    parent.personsToTopics(pid1) = parent.personsToTopics(pid1) - tid1 + tid2 // TODO Not a fan of this
    parent.personsToTopics(pid2) = parent.personsToTopics(pid2) + tid1 - tid2 // TODO Not a fan of this
    invalidateCacheForPersons(pid1, pid2)
  }

  /** Persons that are present on this slot and can be moved around with the current topics being planned (they're not
   * mandatory on their current topic). */
  def mobilePersons: SmallIdSet[PersonId] = {
    problem.slotsPersonsPresent(slot).filter { pid =>
      val tid = personsToTopic(pid)
      !problem.isPersonMandatory(pid, tid)
    }
  }

  /* ALL SCORING STUFF */

  private var cacheNeedsRecalculation: Boolean = true
  private val cachePersonsScore: IdMap[PersonId, Score] = IdMap.fill[PersonId, Score](Score.Missing)

  def invalidateCacheForPersons(pids: PersonId*): Unit = {
    cacheNeedsRecalculation = true
    pids.fastForeach { pid =>
      cachePersonsScore(pid) = Score.Missing
      parent.invalidateCacheForPerson(pid)

      /* Person that had a wish on this person have their score changed */
      val otherPersons: SmallIdSet[PersonId] = problem.personsTargetedToPersonsWithWish(pid)
      otherPersons.foreach { pid =>
        cachePersonsScore(pid) = Score.Missing
        parent.invalidateSlotCacheForPerson(pid) // no need to recalculate global score for them, just slot-level is enough
      }
    }
  }

  def getPersonScore(pid: PersonId): Score = {
    recalculateIfNeeded()
    cachePersonsScore(pid)
  }

  private def recalculateIfNeeded(): Unit = if (cacheNeedsRecalculation) {
    // TODO to ultra-optimize this, we could have all person scores as a single array: first person/topic, then person/person.
    this.personsToTopic.foreach { (pid, tid) =>
      if (cachePersonsScore(pid) == Score.Missing) {
        cachePersonsScore(pid) = scoreWishes(problem, pid, tid)
      }
    }
    cacheNeedsRecalculation = false
  }

  private def scoreWishes(problem: SmallProblem, pid: PersonId, tid: TopicId): Score = {
    /* Missing person gets a score of zero */
    // TODO add a test for this
    if (tid == TopicId.None) Score.Zero else {
      val topicsScore: Score = problem.prefsPersonTopic(pid, tid)
      val otherPersonsScore: Score =
        if (!problem.personsWithPersonWish.contains(pid)) Score.Zero
        else if (tid.value < problem.unassignedTopicsCount.value) Score.Zero // Person sym/antipathy doesn't apply on unassigned topics
        else (topicsToPersons(tid) - pid).mapSumToScore(problem.prefsPersonPerson(pid, _))
      topicsScore + otherPersonsScore
    }
  }
}

object SlotAssignment {
  def empty(problem: SmallProblem, slotId: SlotId): SlotAssignment = {
    import problem.given
    SlotAssignment(problem, slotId, IdMap.fill[PersonId, TopicId](TopicId.None))
  }
}
