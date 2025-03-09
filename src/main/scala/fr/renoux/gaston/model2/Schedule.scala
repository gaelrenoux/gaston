package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{fastFoldRight, fastForeach, fastLoop, testOnly}


// TODO for all of those, check if having an Array isn't better than having a SmallIdSet
// TODO most fields should be private, if not all

/**
 *
 * Rescoring (recalculating scores) is normally handled eagerly, on any change made. If explicitly required on a change,
 * no rescoring is done, in which case this class' user must manually trigger a rescoring.
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

  private var totalScore: Score = Score.Missing
  private val personsNonSlotScores: IdMap[PersonId, Score] = IdMap.fill[PersonId, Score](Score.Missing)
  private val personsScores: IdMap[PersonId, Score] = IdMap.fill[PersonId, Score](Score.Missing)
  private var personsScore: Score = Score.Missing
  private var topicsPureScore: Score = Score.Missing

  /** Has to be remade on every recalculation, because the last step of the recalculation is a destructive sort */
  private val personsScoresSorted: Array[Score] = Array.fill(problem.personsCount.value)(Score.Missing)

  /* Allow to save score to restore them during an undo */
  private var savedTotalScore: Score = Score.Missing
  private val savedPersonsNonSlotScores: IdMap[PersonId, Score] = IdMap.fill[PersonId, Score](Score.Missing)
  private val savedPersonsScores: IdMap[PersonId, Score] = IdMap.fill[PersonId, Score](Score.Missing)
  private var savedPersonsScore: Score = Score.Missing
  private var savedTopicsPureScore: Score = Score.Missing


  def getTotalScore(): Score = {
    totalScore
  }

  @testOnly
  def getPersonScores(): IdMap[PersonId, Score] = {
    personsScores
  }

  @testOnly
  def getTopicPersonNames(topic: TopicId): Set[String] = {
    val slot = topicsToSlot(topic)
    if (slot == SlotId.None) Set.empty
    else {
      val sa = slotsToAssignment(slot)
      sa.topicsToPersons(topic).toSet.map(problem.personsName.apply).toSet
    }
  }

  def saveScores(): Unit = {
    savedTotalScore = totalScore
    savedPersonsNonSlotScores.fillFrom(personsNonSlotScores)
    savedPersonsScores.fillFrom(personsScores)
    savedPersonsScore = personsScore
    savedTopicsPureScore = topicsPureScore
  }

  def restoreSavedScores(): Unit = {
    totalScore = savedTotalScore
    personsNonSlotScores.fillFrom(savedPersonsNonSlotScores)
    personsScores.fillFrom(savedPersonsScores)
    personsScore = savedPersonsScore
    topicsPureScore = savedTopicsPureScore
  }

  // TODO I'm not a fan of the reversed logic between recalculateAll and recalculateScoreFor. In the first case, the
  //  schedule calls the slot-assignment, while in the second the slot-assignment calls the schedule. I'm thinking I
  //  should concentrate all external methods at the schedule level.

  /** Calls the recalculateAll of the slot-assignment */
  def recalculateAll(): Unit = {
    recalculateTopicScore()
    slotsToAssignment.foreach { (slot, sa) =>
      sa.recalculateAll()
    }
    problem.personsCount.foreach { p =>
      _recalculatePersonScoreFor(p, includeNonSlot = true)
      personsScoresSorted(p.value) = personsScores(p)
    }
    Score.sort(personsScoresSorted)
    personsScore = personsScoresSorted.fastFoldRight(Score.Zero) { (score, acc) =>
      (SmallProblem.RankFactor * acc: Score) + score
    }
    totalScore = personsScore + topicsPureScore
  }

  /**
   * Assumes the recalculation has already been done on the slot-assignment level
   * @param otherPersons No need to recalculate global score for them, just slot-level is enough
   */
  def recalculateScoreFor(person: PersonId, otherPersons: SmallIdSet[PersonId]): Unit =
    recalculateScoreFor(person, PersonId.None, otherPersons)

  /**
   * Assumes the recalculation has already been done on the slot-assignment level
   * @param otherPersons No need to recalculate global score for them, just slot-level is enough
   */
  def recalculateScoreFor(person1: PersonId, person2: PersonId, otherPersons: SmallIdSet[PersonId]): Unit = {
    _recalculatePersonScoreFor(person1, includeNonSlot = true)
    if (person2 != PersonId.None) {
      _recalculatePersonScoreFor(person2, includeNonSlot = true)
    }
    otherPersons.foreach { pid =>
      _recalculatePersonScoreFor(pid, includeNonSlot = false)
    }

    // Possibility to check if that's a good idea here, before sorting
    // val maybeUseful = problem.personsCount.exists { pid => personsScores(pid) > previousPersonScores(pid) }
    // if (!maybeUseful) {
    //   problem.personsCount.foreach { pid => personsScores(pid) = previousPersonScores(pid) }
    //   false
    // } else {
    //   problem.personsCount.foreach { pid => previousPersonScores(pid) = personsScores(pid) }
    //   ...

    problem.personsCount.foreach { p =>
      personsScoresSorted(p.value) = personsScores(p)
    }
    Score.sort(personsScoresSorted)
    personsScore = personsScoresSorted.fastFoldRight(Score.Zero) { (score, acc) =>
      (SmallProblem.RankFactor * acc: Score) + score
    }

    totalScore = personsScore + topicsPureScore

  }

  /** Scores a person, storing the result in personsScore and returning it. */
  private def _recalculatePersonScoreFor(person: PersonId, includeNonSlot: Boolean): Unit = {
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
        val baseScore = problem.personsBaseScore(person)
        val exclusiveScore = problem.prefsTopicsExclusive(person).evaluate(topicIds)
        val linkedScore = getScoreLinked(topicIds)
        val s = baseScore + exclusiveScore + linkedScore
        personsNonSlotScores(person) = s
        s
      } else {
        personsNonSlotScores(person)
      }

    personsScores(person) = nonSlotScore + slotsScore
  }

  def recalculateTopicScore(): Unit = {
    topicsPureScore = this.topicsPresent.mapSumToScore(problem.prefsTopicPure(_))
    totalScore = personsScore + topicsPureScore
  }

  private def getScoreLinked(topicIds: SmallIdSet[TopicId]) = {
    var linkedScore = Score.Zero
    problem.prefsTopicsLinked.fastForeach { linked =>
      val result = linked && topicIds
      if (result.nonEmpty && result != linked) {
        linkedScore += Score.MinReward.value * result.size.value
      }
    }
    linkedScore
  }

  /** Verifies if the schedule is consistent. Very useful in tests. Poor performance. */
  def slowCheckup: List[String] = {
    // TODO Add more controls
    slotsToAssignment.valuesSeq.view.flatMap { sa => sa.slowCheckup.map(s"Slot ${sa.slot} (${problem.slotsNames(sa.slot)}): " + _) }.toList
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
