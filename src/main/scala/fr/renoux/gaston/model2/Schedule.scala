package fr.renoux.gaston.model2

import fr.renoux.gaston.util.testOnly
import fr.renoux.gaston.util.fastForeach
import fr.renoux.gaston.util.fastFoldRight

import java.util as jutil


// TODO for all of those, check if having an Array isn't better than having a SmallIdSet
// TODO most fields should be private, if not all
class Schedule(
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
    this
  }

  inline def removeTopic(tid: TopicId): Schedule = {
    topicsToSlot(tid) = SlotId.None
    topicsPresent = topicsPresent - tid
    this
  }




  /* ALL SCORING METHODS */

  // TODO Keep a previous version. That way, a revert can just get it back. Only allows for one revert, but that should be enough.

  private var cacheTotalScore: Score = Score.Missing
  private val cachePersonsSlotScore: IdMap[PersonId, Score] = IdMap.fill[PersonId, Score](Score.Missing)
  private val cachePersonsGlobalScore: IdMap[PersonId, Score] = IdMap.fill[PersonId, Score](Score.Missing)
  private val cachePersonsScore: IdMap[PersonId, Score] = IdMap.fill[PersonId, Score](Score.Missing)
  private var cacheTopicsPresentScore: Score = Score.Missing

  private var previousCacheTotalScore: Score = Score.Missing

  def getTotalScore(): Score = {
    recalculateIfNeeded(cacheTotalScore)
    cacheTotalScore
  }

  @testOnly
  private def getPersonScore(pid: PersonId): Score = {
    recalculateIfNeeded(cachePersonsScore(pid))
    cachePersonsScore(pid)
  }

  /** Only used in tests. Pretty slow because it goes over all persons to check their cache status. */
  @testOnly
  def slowGetPersonScores() = {
    IdMap.tabulate[PersonId, Score](getPersonScore)
  }

  def saveCache(): Unit = {
    previousCacheTotalScore = cacheTotalScore
  }
  def restoreCache(): Unit = {
    cacheTotalScore = previousCacheTotalScore
    previousCacheTotalScore = Score.Missing
  }

  private def invalidateCacheTotal(): Unit = {
    cacheTotalScore = Score.Missing
  }

  def invalidateCacheForPerson(personId: PersonId) = {
    invalidateCacheTotal()
    cachePersonsScore(personId) = Score.Missing
    cachePersonsGlobalScore(personId) = Score.Missing
    cachePersonsSlotScore(personId) = Score.Missing
  }

  def invalidateSlotCacheForPerson(personId: PersonId): Unit = {
    invalidateCacheTotal()
    cachePersonsScore(personId) = Score.Missing
    cachePersonsSlotScore(personId) = Score.Missing
  }

  def invalideCacheForTopics(): Unit = {
    invalidateCacheTotal()
    cacheTopicsPresentScore = Score.Missing
  }

  /** Has to be remade on every recalculation, because the last step of the recalculation is a destructive sort */
  private val personScores: Array[Score] = Array.fill(problem.personsCount.value)(Score.Missing)

  private inline def recalculateIfNeeded(checked: Score = cacheTotalScore): Unit = if (checked == Score.Missing) {
    problem.personsCount.foreach { pid =>
      val personTotalScore = scorePerson(pid)
      personScores(pid.value) = personTotalScore
    }
    Score.sort(personScores)
    val personsTotalScore = personScores.fastFoldRight(Score.Zero) { (score, acc) =>
      (SmallProblem.RankFactor * acc: Score) + score
    }

    val topicsPureTotalScore = this.topicsPresent.mapSumToScore(problem.prefsTopicPure(_))

    cacheTotalScore = personsTotalScore + topicsPureTotalScore
  }

  /** Uses the person's base score, slot-assignments, exclusive and linked topics */
  private def scorePerson(pid: PersonId): Score = {
    val cachedPersonTotalScore = cachePersonsScore(pid)
    val personTotalScore = if (cachedPersonTotalScore != Score.Missing) cachedPersonTotalScore else {
      
      val cachedSlotScore = cachePersonsSlotScore(pid)
      val slotsScore = if (cachedSlotScore != Score.Missing) cachedSlotScore else {
        var s = Score.Zero
        slotsToAssignment.foreach { (_, sa) =>
          s += sa.getPersonScore(pid)
        }
        s
      }

      val cachedGlobalScore = cachePersonsGlobalScore(pid)
      val globalScore = if (cachedGlobalScore != Score.Missing) cachedGlobalScore else {
        val topicIds = personsToTopics(pid)
        val baseScore = problem.personsBaseScore(pid)
        val exclusiveScore = problem.prefsTopicsExclusive(pid).evaluate(topicIds)
        val linkedScore = scoreLinked(topicIds)
        val s = baseScore + exclusiveScore + linkedScore
        cachePersonsGlobalScore(pid) = s
        s
      }
      
      val s = globalScore + slotsScore
      cachePersonsScore(pid) = s
      s
    }
    personScores(pid.value) = personTotalScore
    personTotalScore
  }

  private def scoreLinked(topicIds: SmallIdSet[TopicId]) = {
    var linkedScore = Score.Zero
    problem.prefsTopicsLinked.fastForeach { linked =>
      val result = linked && topicIds
      if (result.nonEmpty && result != linked) {
        linkedScore += Score.MinReward.value * result.size.value
      }
    }
    linkedScore
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
