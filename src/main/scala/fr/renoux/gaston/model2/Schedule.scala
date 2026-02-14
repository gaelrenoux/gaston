package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{fastFoldRight, fastForeach, fastLoop, testOnly}


// TODO for all of those, check if having an Array isn't better than having a SmallIdSet
// TODO most fields should be private, if not all

/** Rescoring (recalculating scores) is normally handled eagerly, on any change made. If explicitly required on a change,
  * no rescoring is done, in which case this class' user must manually trigger a rescoring.
  *
  * Mutable.
  */
final class Schedule(
    val problem: SmallProblem,
    val slotsToAssignment: IdMap[SlotId, SlotAssignment],
    val personsToTopics: IdMap[PersonId, SmallIdSet[TopicId]],
    val topicsToSlot: IdMap[TopicId, SlotId]
) {

  import problem.given

  var topicsPresent: SmallIdSet[TopicId] = topicsToSlot.keysFilter { (_, sid) => sid != SlotId.None }

  def slotsToTopics: IdMap[SlotId, SmallIdSet[TopicId]] = topicsToSlot.transpose

  inline def addTopic(sid: SlotId, tid: TopicId): Schedule = {
    topicsToSlot(tid) = sid
    topicsPresent = topicsPresent + tid
    recalculateTopicScore()
    this
  }

  inline def removeTopic(tid: TopicId): Schedule = {
    topicsToSlot(tid) = SlotId.None
    topicsPresent = topicsPresent - tid
    recalculateTopicScore()
    this
  }

  /* ALL SCORING STUFF */

  /** This contains the ENTIRE score. It is the sum of the personsTotalScore and the topicsTotalScore */
  private var totalScore: Score = Score.Missing
  /** This contains, for each person, their score that cannot be tied a slot (for example: unrespected linked topics). */
  private val personsToNonSlotScore: IdMap[PersonId, Score] = IdMap.fill[PersonId, Score](Score.Missing)
  /** This contains, for each person, their score (including both the non-slot score from the previous field, and slot score) */
  private val personsToScore: IdMap[PersonId, Score] = IdMap.fill[PersonId, Score](Score.Missing)
  /** This contains the total score coming from persons (so is calculated from personsToScore by applying the RankFactor) */
  private var personsTotalScore: Score = Score.Missing
  /** This contains the total score coming from topics regardless of persons (topics with a base value). */
  private var topicsTotalScore: Score = Score.Missing

  /** This must be refilled on every recalculation, because the last step of the recalculation is a destructive sort */
  private val personsScoresSorted: Array[Score] = Array.fill(problem.personsCount.value)(Score.Missing)

  /* The following fields are used to save scores, in order to restore them after an undo. We only save score that are
  * stored at the schedule level, the SlotAssignment saves its own scores. */
  private var savedTotalScore: Score = Score.Missing
  private val savedPersonsToNonSlotScore: IdMap[PersonId, Score] = IdMap.fill[PersonId, Score](Score.Missing)
  private val savedPersonsToScore: IdMap[PersonId, Score] = IdMap.fill[PersonId, Score](Score.Missing)
  private var savedPersonsTotalScore: Score = Score.Missing
  private var savedTopicsTotalScore: Score = Score.Missing

  /** Persons who have been moved and whose score has changed on the assignment, but has not been recalculated on the schedule yet. */
  private var personsWithPendingScoreChanges: SmallIdSet[PersonId] = SmallIdSet.empty[PersonId]

  /** Same as personsWithPendingScoreChanges, except those persons haven't moved (but their groups have, so they might have new scores due to person-to-person preferences). */
  private var personsUnmovedWithPendingScoreChanges: SmallIdSet[PersonId] = SmallIdSet.empty[PersonId]

  private inline def assertNoPendingChanges(): Unit = {
    assert(personsWithPendingScoreChanges.isEmpty && personsUnmovedWithPendingScoreChanges.isEmpty, "There should be no pending changes")
  }

  private inline def assertPendingChanges(): Unit = {
    assert(personsWithPendingScoreChanges.nonEmpty || personsUnmovedWithPendingScoreChanges.isEmpty, "There should be some pending changes")
  }

  def getTotalScore: Score = {
    assertNoPendingChanges()
    totalScore
  }

  def getPersonsTotalScore: Score = {
    assertNoPendingChanges()
    personsTotalScore
  }

  def getTopicsTotalScore: Score = {
    assertNoPendingChanges()
    topicsTotalScore
  }

  /** This returns directly the inner value. Do NOT modify it. */
  def getPersonsToScore: IdMap[PersonId, Score] = {
    assertNoPendingChanges()
    personsToScore
  }

  @testOnly
  def getTopicPersonNames(topic: TopicId): Set[String] = {
    val slot = topicsToSlot(topic)
    if (slot == SlotId.None) Set.empty
    else {
      val sa = slotsToAssignment(slot)
      sa.topicsToPersons(topic).toSet.map(problem.personsToName.apply).toSet
    }
  }

  /** Save all scores for this Schedule */
  def saveScores(): Unit = {
    saveLocalScores()
    slotsToAssignment.foreachValue(_.saveLocalScores())
  }

  /** Only save locally stored scores */
  private[model2] def saveLocalScores(): Unit = {
    assertNoPendingChanges()

    savedTotalScore = totalScore
    savedPersonsToNonSlotScore.fillFrom(personsToNonSlotScore)
    savedPersonsToScore.fillFrom(personsToScore)
    savedPersonsTotalScore = personsTotalScore
    savedTopicsTotalScore = topicsTotalScore
  }

  def restoreSavedScores(): Unit = {
    restoreSavedLocalScores()
    slotsToAssignment.foreachValue(_.restoreSavedLocalScores())
  }

  private def restoreSavedLocalScores(): Unit = {
    totalScore = savedTotalScore
    /* Note: can't just switch values. We wanted saved score to be available for another restore if needed. */
    personsToNonSlotScore.fillFrom(savedPersonsToNonSlotScore)
    personsToScore.fillFrom(savedPersonsToScore)
    personsTotalScore = savedPersonsTotalScore
    topicsTotalScore = savedTopicsTotalScore

    personsWithPendingScoreChanges = SmallIdSet.empty
    personsUnmovedWithPendingScoreChanges = SmallIdSet.empty
  }

  /** Register that some persons have changes in score, and therefore the global score needs a recalculation */
  def registerPendingChanges(movedPersons: SmallIdSet[PersonId], unmovedPersons: SmallIdSet[PersonId]): Unit = {
    personsWithPendingScoreChanges = personsWithPendingScoreChanges ++ movedPersons
    personsUnmovedWithPendingScoreChanges = personsUnmovedWithPendingScoreChanges -- movedPersons ++ unmovedPersons
  }

  /** Calls the recalculateAll of the slot-assignments, and recalculate everything in the schedule as well. */
  def recalculateAll(): Unit = {
    recalculateTopicScore()
    slotsToAssignment.foreach { (slot, sa) =>
      sa.recalculateAll()
    }
    problem.personsCount.foreach { p =>
      recalculatePersonScoreFor(p, includeNonSlot = true)
    }
    recalculatePersonScoreTotalOnly()

    personsWithPendingScoreChanges = SmallIdSet.empty
    personsUnmovedWithPendingScoreChanges = SmallIdSet.empty
  }

  private def recalculateTopicScore(): Unit = {
    topicsTotalScore = this.topicsPresent.mapSumToScore(problem.prefsTopicPure(_))
    totalScore = personsTotalScore + topicsTotalScore
  }

  /** Recalculate the score following all pending changes registered on persons.
    * Note that the recalculation must already have been done on the slot-assignment level (otherwise, there would be no
    * pending changes).
    *
    * For persons who didn't switch topic, we don't need to recalculate everything, as the only thing that might have
    * changed is the appreciation of the persons they are with.
    */
  def recalculateScoreForPersonsPendingChanges(): Unit = {
    assertPendingChanges()

    personsWithPendingScoreChanges.foreach { pid =>
      recalculatePersonScoreFor(pid, includeNonSlot = true)
    }
    personsUnmovedWithPendingScoreChanges.foreach { pid =>
      recalculatePersonScoreFor(pid, includeNonSlot = false)
    }
    personsWithPendingScoreChanges = SmallIdSet.empty
    personsUnmovedWithPendingScoreChanges = SmallIdSet.empty

    // Possibility to check if that's a good idea here, before sorting
    // val maybeUseful = problem.personsCount.exists { pid => personsScores(pid) > previousPersonScores(pid) }
    // if (!maybeUseful) {
    //   problem.personsCount.foreach { pid => personsScores(pid) = previousPersonScores(pid) }
    //   false
    // } else {
    //   problem.personsCount.foreach { pid => previousPersonScores(pid) = personsScores(pid) }
    //   ...

    recalculatePersonScoreTotalOnly()
  }

  /** Assumes that the person-by-person score is OK.
    * Recalculates the total person score (and the corresponding total score) based on those. */
  private def recalculatePersonScoreTotalOnly(): Unit = {
    problem.personsCount.foreach { p =>
      personsScoresSorted(p.value) = personsToScore(p)
    }
    Score.sort(personsScoresSorted)
    personsTotalScore = personsScoresSorted.fastFoldRight(Score.Zero) { (score, acc) =>
      (SmallProblem.RankFactor * acc: Score) + score
    }

    totalScore = personsTotalScore + topicsTotalScore
  }

  /** Scores a person, storing the result in personsScore. */
  private def recalculatePersonScoreFor(person: PersonId, includeNonSlot: Boolean): Unit = {
    val slotsScore = {
      var s = Score.Zero
      slotsToAssignment.foreach { (_, sa) =>
        s += sa.getPersonScore(person)
      }
      s
    }

    val nonSlotScore =
      if (includeNonSlot) {
        val topicIds = personsToTopics(person)
        val baseScore = problem.personsToBaseScore(person)
        val exclusiveScore = problem.prefsTopicsExclusive(person).evaluate(topicIds)
        val linkedScore = problem.scoreForPrefsTopicsLinked(topicIds)
        val s = baseScore + exclusiveScore + linkedScore
        personsToNonSlotScore(person) = s
        s
      } else {
        personsToNonSlotScore(person)
      }

    personsToScore(person) = nonSlotScore + slotsScore
  }

  /** Verifies if the schedule is consistent. Very useful in tests. Poor performance. */
  def slowCheckup: List[String] = {
    // TODO Add more controls
    slotsToAssignment.valuesSeq.view.flatMap { sa => sa.slowCheckup.map(s"Slot ${sa.slot} (${problem.slotsToName(sa.slot)}): " + _) }.toList
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: Schedule =>
      problem == that.problem &&
          slotsToAssignment.actualEquals(that.slotsToAssignment) &&
          personsToTopics.actualEquals(that.personsToTopics) &&
          topicsToSlot.actualEquals(that.topicsToSlot)
    case _ => false
  }


  /* TEST METHODS */

  @testOnly
  def on(slot: SlotId)(f: SlotAssignment => Any): Schedule = {
    val _ = f(slotsToAssignment(slot))
    this
  }


  //  /* OLD STUFF */
  //
  //  def score(problem: SmallProblem): Score = ???
  //
  //  def scorePersons(problem: SmallProblem): IdMap[PersonId, Score] = ???


  /* INITIALIZATION */

  slotsToAssignment.foreach { (_, sa) =>
    sa.parent = this
  }
}

object Schedule {
  def empty(problem: SmallProblem): Schedule = {
    import problem.given
    Schedule(
      problem,
      IdMap.tabulate[SlotId, SlotAssignment](SlotAssignment.empty(problem, _)),
      IdMap.fill[PersonId, SmallIdSet[TopicId]](SmallIdSet.empty[TopicId]),
      IdMap.fill[TopicId, SlotId](SlotId.None)
    )
  }
}
