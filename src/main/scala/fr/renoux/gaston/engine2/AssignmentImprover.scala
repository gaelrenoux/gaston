package fr.renoux.gaston.engine2

import fr.renoux.gaston.model2.*
import fr.renoux.gaston.util.Context
import fr.renoux.gaston.util.Context.chrono

import scala.math.Ordering.Implicits.infixOrderingOps
import scala.util.Random


/** Improves an existing Schedule by moving persons around. Does not reschedule topics, or remove them. Every person
  * should already be assigned somewhere (this will not assign unassigned persons).
  */
final class AssignmentImprover(private val problem: SmallProblem)(using private val ctx: Context) {

  import problem.given

  private val defaultMaxRoundsCount = 1000

  /** This value is to avoid one slot hugging all round to improve itself, leaving no rounds to the other slots. */
  private val defaultMaxSuccessiveRoundsCountPerSlot = defaultMaxRoundsCount / problem.slotsCount.value

  /** Main method. Modifies the schedule to make it better (mutates the argument).
    *
    * Ends either because the schedule can't be perfected any more or because the limit number of rounds has been
    * reached.
    */
  def improve(schedule: Schedule, maxRounds: Int = defaultMaxRoundsCount)(using Random) = 
    chrono("PersonPlacementImprover >  improve") {
      val randTopicIds = schedule.topicsPresent.toShuffledArray
      val personIds = problem.personsCount.shuffled

      var personIx = 0
      var blockedPersons = 0
      var roundsLeft = maxRounds

      while (blockedPersons < problem.personsCount.value && roundsLeft > 0) {
        val pid = personIds(personIx)
        if (goodChangeForPerson(schedule, pid, randTopicIds)) {
          blockedPersons = 0
        } else {
          blockedPersons += 1
        }

        personIx = (personIx + 1) % problem.personsCount.value
        roundsLeft -= 1
      }
    }

  /** Try to improve the schedule for that person */
  private def goodChangeForPerson(schedule: Schedule, pid: PersonId, randTopicIds: Array[TopicId]): Boolean = {
      var targetTopicIx = 0
      var found = false

      while (!found && targetTopicIx < randTopicIds.length) {
        val targetTid = randTopicIds(targetTopicIx)
        val targetTopicPersons = schedule.topicsToPersons(targetTid)
        if (!targetTopicPersons.contains(pid)) {
          // TODO If a person is locked for a slot because it's on a mandatory topic, we're still going to try to move them => let's go slot by slot instead
          found = goodChangeForPersonTopic(schedule, pid, targetTid, targetTopicPersons)
        }
        targetTopicIx += 1
      }

      found
  }

  private def goodChangeForPersonTopic(schedule: Schedule, pid: PersonId, targetTid: TopicId, targetTopicPersons: SmallIdSet[PersonId]): Boolean = {
    var found = false
    val sid = schedule.topicsToSlot(targetTid)
    val currentTid = schedule.topicOf(sid, pid)

    if (!problem.isPersonMandatory(pid, currentTid)) {

      /* First, let's see if we can just move the person onto the target topic */
      if (schedule.isDroppableTopic(problem, pid, currentTid) && schedule.isAddableTopic(problem, pid, targetTid)) {
        /* We can just move that person on the target topic */
        val oldScore = schedule.score(problem)
        schedule.move(pid, currentTid, targetTid)
        val newScore = schedule.score(problem)
        if (newScore <= oldScore) {
          val _ = schedule.reverseMove(pid, currentTid, targetTid)
        } else {
          // found a good one
          found = true
        }
      }
      
      /* If moving the person wasn't possible or didn't improve the score, we'll try to swap the person with another one */
      if (!found) {
        targetTopicPersons.foreachWhile { otherPid =>
          if (!problem.isPersonMandatory(otherPid, targetTid)) {
            val oldScore = schedule.score(problem)
            schedule.move(pid, currentTid, targetTid)
            schedule.move(otherPid, targetTid, currentTid)
            val newScore = schedule.score(problem)
            if (newScore <= oldScore) {
              schedule.reverseMove(otherPid, targetTid, currentTid)
              schedule.reverseMove(pid, currentTid, targetTid)
              ()
            } else {
              // found a good one
              found = true
            }
          }
          !found
        }
      }
    }
    
    found
  }

  // TODO From previous version:
  // - We might be examining swaps twice, once from each side. Can we exclude that?
  // - I might need to bring forbidden back as a constraint, to avoid evaluating the score when it's never gonna help.

}
