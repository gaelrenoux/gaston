package fr.renoux.gaston.model2

import fr.renoux.gaston.util.*

import scala.compiletime.uninitialized
import scala.util.Random


/** Assignments for a single slot (who is on what topic).
  *
  * Rescoring (recalculating scores) is normally handled eagerly, on any change made. If explicitly required on a change,
  * no rescoring is done, in which case this class' user must manually trigger a rescoring.
  */
final class SlotAssignment(
    val problem: SmallProblem,
    val slot: SlotId,
    val personsToTopic: IdMap[PersonId, TopicId], // Default value (for persons that aren't there) is TopicId.None
) {

  import problem.given

  var parent: Schedule = uninitialized // Will be set up on initialization of the schedule

  /** Topics currently scheduled on that assignment */
  var scheduledTopics: SmallIdSet[TopicId] = uninitialized // Will be set up on initialization of the schedule

  // TODO for all of those, check if having an Array isn't better than having a SmallIdSet
  val topicsToPersons: IdMap[TopicId, SmallIdSet[PersonId]] = personsToTopic.transpose

  /** Returns open topics currently planned on this assignment */
  def getOpenTopics: SmallIdSet[TopicId] = {
    scheduledTopics.filter { tid =>
      topicsToPersons(tid).size < problem.topicsToMaxPersons(tid)
    }
  }

  /** Returns a possible target topic for a person. It's allowed, planned on this slot, and if possible the person likes it. Never a linked topic. */
  def pickGoodOpenTopicFor(pid: PersonId)(using Random): TopicId = {
    val openTopics = getOpenTopics
    val bestTopics = problem.personsToLikedTopics(pid) && openTopics && problem.unlinkedTopics
    if (bestTopics.nonEmpty) {
      return bestTopics.pickRandom
    }
    val okTopics = problem.personsToNonHatedTopics(pid) && openTopics && problem.unlinkedTopics
    if (okTopics.nonEmpty) {
      return okTopics.pickRandom
    }
    problem.unassignedTopic(slot)
  }

  /** Returns true if that person can be added to this topic, without moving anyone else. Checks if the maximum number of persons is respected. */
  def isAddableToTopic(pid: PersonId, tid: TopicId): Boolean = {
    inline def topicMax = problem.topicsToMaxPersons(tid)

    inline def topicPersonsCount = topicsToPersons(tid).size
    // TODO should handle forbidden here
    topicPersonsCount < topicMax
  }

  /** Returns true if that person can be removed from that topic, without moving anyone else. Checks if that person is mandatory, and if the minimum number of persons is respected. */
  def isDroppableFromTopic(pid: PersonId, tid: TopicId): Boolean = {
    inline def topicMin = problem.topicsToMinPersons(tid)

    inline def topicPersonsCount = topicsToPersons(tid).size

    inline def personIsMandatory = problem.isPersonMandatory(pid, tid)

    !personIsMandatory && topicPersonsCount > topicMin
  }

  /** Moves one person from one topic to another.
    * The score will be recalculated at the assignment level, and a notification will be sent to the schedule. */
  def move(pid: PersonId, tid1: TopicId, tid2: TopicId): SlotAssignment = {
    // TODO Dev mode control: pid should be on tid1
    _move(pid, tid1, tid2)
    val otherPersons = recalculateScoreFor(pid, tid2)
    parent.registerPendingChanges(SmallIdSet(pid), otherPersons)
    this
  }

  /** Undo a move. The score will not be recalculated at all, as we expect a restore of saved scores later down the line. */
  def undoMove(pid: PersonId, tid1: TopicId, tid2: TopicId): SlotAssignment = {
    _move(pid, tid2, tid1)
    this
  }

  private inline def _move(pid: PersonId, tid1: TopicId, tid2: TopicId): Unit = {
    // TODO Dev mode control: pid should be on tid1
    personsToTopic(pid) = tid2
    topicsToPersons(tid1) = topicsToPersons(tid1) - pid
    topicsToPersons(tid2) = topicsToPersons(tid2) + pid
    parent.personsToTopics(pid) = parent.personsToTopics(pid) - tid1 + tid2 // TODO Not a fan of this
  }

  /** Swaps two persons between two topics.
    * First we have the first person and their current topic, then the second person and their current topic.
    * The score will be recalculated at the assignment level, and a notification will be sent to the schedule.
    */
  def swap(pid1: PersonId, tid1: TopicId, pid2: PersonId, tid2: TopicId): SlotAssignment = {
    // TODO Dev mode control: pid1 should be on tid1, pid2 should be on tid2
    _swap(pid1, tid1, pid2, tid2)
    val otherPersons = recalculateScoreFor(pid1, tid2, pid2, tid1)
    parent.registerPendingChanges(SmallIdSet(pid1, pid2), otherPersons)
    this
  }

  /** Undo a swap. The score will not be recalculated at all, as we expect a restore of saved scores later down the line. */
  def undoSwap(pid1: PersonId, tid1: TopicId, pid2: PersonId, tid2: TopicId): SlotAssignment = {
    _swap(pid1, tid2, pid2, tid1)
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
  }

  /** Persons that are present on this slot and can be moved around with the current topics being planned (they're not
    * mandatory on their current topic). */
  def mobilePersons: SmallIdSet[PersonId] = {
    problem.slotsToPersonsPresent(slot).filter { pid =>
      val tid = personsToTopic(pid)
      !problem.isPersonMandatory(pid, tid)
    }
  }

  /* ALL SCORING STUFF */

  private val personsToScore: IdMap[PersonId, Score] = IdMap.fill[PersonId, Score](Score.Missing)
  private val savedPersonsToScore: IdMap[PersonId, Score] = IdMap.fill[PersonId, Score](Score.Missing)

  def getPersonScore(pid: PersonId): Score = {
    personsToScore(pid)
  }

  def saveScores(): Unit = {
    saveLocalScores()
    parent.saveLocalScores()
  }

  /** Save locally stored scores for this assignment */
  private[model2] def saveLocalScores(): Unit = {
    savedPersonsToScore.fillFrom(personsToScore)
  }

  /** Restore locally stored scores for this assignment */
  private[model2] def restoreSavedLocalScores(): Unit = {
    /* Note: can't just switch values. We wanted saved score to be available for another restore if needed. */
    personsToScore.fillFrom(savedPersonsToScore)
  }

  def recalculateAll(): Unit = {
    problem.personsCount.foreach { person =>
      val topic = personsToTopic(person)
      personsToScore(person) = scoreWishesFor(person, topic)
    }
  }

  /** Recalculates the score for one or two persons, each on their new topics.
    * Returns the other persons impacted by the primary person's presence or not (their scores have been recalculated as well).
    */
  def recalculateScoreFor(person1: PersonId, topic1: TopicId, person2: PersonId = PersonId.None, topic2: TopicId = TopicId.None): SmallIdSet[PersonId] = {
    personsToScore(person1) = scoreWishesFor(person1, topic1)
    if (person2 != PersonId.None) {
      personsToScore(person2) = scoreWishesFor(person2, topic2)
    }

    /* Person that had a wish on these persons have their score changed, even though they did not change topics */
    val otherPersons: SmallIdSet[PersonId] =
      if (person2 == PersonId.None) problem.personsTargetedToPersonsWithWish(person1)
      else (problem.personsTargetedToPersonsWithWish(person1) ++ problem.personsTargetedToPersonsWithWish(person2)) - person1 - person2
    otherPersons.foreach { p =>
      val topic = personsToTopic(p)
      personsToScore(p) = scoreWishesFor(p, topic)
      // TODO if I split the score due to the topic and the score due to the persons on the topic, I can only recalculate the latter here
    }
    otherPersons
  }

  /** Score the wishes for a specific person, on specific topic (including wishes for the topic, but also wishes for persons on that topic) */
  private def scoreWishesFor(pid: PersonId, tid: TopicId): Score = {
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


  /* CHECKUPS */

  /** Verifies if the schedule is consistent. Very useful in tests. Poor performance.
    * @return A list of errors.
    */
  def slowCheckup: List[String] = {
    slowCheckupUnassignedTopics ++ slowCheckupTopicToPersons
    // Other checks to come
  }

  def slowCheckupUnassignedTopics: List[String] = {
    val topicsHere: Set[TopicId] = topicsToPersons.toMap.filter(_._2.nonEmpty).keySet
    val unassignedTopicsHere = topicsHere.filter(problem.isTopicUnassigned)
    if (unassignedTopicsHere.isEmpty || unassignedTopicsHere == Set(slot.value)) Nil else {
      val topicsStr = unassignedTopicsHere.toSeq.sorted.map { tid =>
        s"$tid (${problem.topicsToName(tid)})"
      }.mkString(" ; ")
      List(s"Unexpected unassigned topics: $topicsStr")
    }
  }

  def slowCheckupTopicToPersons: List[String] = {
    val ttp: Map[TopicId, Seq[PersonId]] = topicsToPersons.toMap.filter(_._2.nonEmpty).view.mapValues(_.toSet.toSeq.sorted).toMap
    val tps: Seq[(TopicId, PersonId)] = ttp.toSeq.flatMap { case (tid, pids) => pids.map(tid -> _) }.sorted
    val ptt: Map[PersonId, TopicId] = personsToTopic.toMap.filterNot(_._2 == TopicId.None)
    val referenceTps: Seq[(TopicId, PersonId)] = ptt.toSeq.map(_.swap).sorted

    if (tps == referenceTps) Nil else {
      val ttpString = ttp.toSeq.sortBy(_._1).map {
        case (tid, pids) => s"${problem.topicsToName(tid)} -> (${pids.map(problem.personsToName.apply).mkString(", ")})"
      }.mkString(" | ")
      val convertedPttString = referenceTps.groupBy(_._1).view.mapValues(_.map(_._2)).toSeq.sortBy(_._1).map {
        case (tid, pids) => s"${problem.topicsToName(tid)} -> (${pids.map(problem.personsToName.apply).mkString(", ")})"
      }.mkString(" | ")
      List(
        s"""[slot $slot] topicsToPersons and personsToTopic incoherence.
           |Tuples compare:
           |\t$tps
           |\t$referenceTps
           |As a map from topics to persons:
           |\t$ttpString
           |\t$convertedPttString
           |--------------------------------------------------------------------------------
           |""".stripMargin.trim)
    }
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: SlotAssignment =>
      problem == that.problem &&
          slot == that.slot &&
          personsToTopic.actualEquals(that.personsToTopic)
    case _ => false
  }
}

object SlotAssignment {
  def empty(problem: SmallProblem, slotId: SlotId): SlotAssignment = {
    import problem.given
    SlotAssignment(problem, slotId, IdMap.fill[PersonId, TopicId](TopicId.None))
  }
}
