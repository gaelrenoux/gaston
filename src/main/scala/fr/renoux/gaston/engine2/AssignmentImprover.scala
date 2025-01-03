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
      val slotsToChangeablePersons: IdMap[SlotId, Array[PersonId]] =
        IdMap.tabulate[SlotId, Array[PersonId]] { sid =>
          val personsWhoCanMove = problem.slotsPersonsPresent(sid).filter { pid =>
            val tid = schedule.topicOf(sid, pid)
            !problem.isPersonMandatory(pid, tid)
          }
          personsWhoCanMove.toShuffledArray
        }
      val slotsToTopics: IdMap[SlotId, Array[TopicId]] = schedule.slotsToTopics.mapValues(_.toShuffledArray)

      var roundsLeft = maxRounds
      var sid: SlotId = 0
      var blockedSlots = 0

      while(blockedSlots < problem.slotsCount && roundsLeft > 0) { // iterate over slots
        val randPersonIds = slotsToChangeablePersons(sid)
        val randTopicIds = slotsToTopics(sid)

        var changesMade = false
        var subRoundsLeft = math.min(roundsLeft, defaultMaxSuccessiveRoundsCountPerSlot)
        var personIx = 0
        var blockedPersons = 0

        while (blockedPersons < randPersonIds.length && subRoundsLeft > 0) { // iterate over persons
          val pid = randPersonIds(personIx)
          var targetTopicIx = 0
          var foundGoodChange = false
          while (!foundGoodChange && targetTopicIx < randTopicIds.length) {
            val targetTid = randTopicIds(targetTopicIx)
            foundGoodChange = goodChangeForPersonTopic(schedule, pid, targetTid)
            targetTopicIx += 1
          }

          if (foundGoodChange) {
            // We changed something on the current slot, making new changes possible
            blockedPersons = 0
            changesMade = true
          } else {
            // Can't improve the current person anymore, until something else changes
            blockedPersons += 1
          }

          personIx = (personIx + 1) % randPersonIds.length
          subRoundsLeft -= 1
          roundsLeft -= 1
        }

        if (changesMade) {
            // We changed something on the current slots, making new changes possible (but this slot is done pending further changes)
          blockedSlots = 1
        } else {
            // Couldn't improve the current slot
          blockedSlots += 1
        }

        sid = sid.next
      }


    }


  private def goodChangeForPersonTopic(schedule: Schedule, pid: PersonId, targetTid: TopicId): Boolean = {
    val targetTopicPersons = schedule.topicsToPersons(targetTid)
    if (targetTopicPersons.contains(pid)) false
    else {
      var found = false
      val sid = schedule.topicsToSlot(targetTid)
      val currentTid = schedule.topicOf(sid, pid)

      if (currentTid != TopicId.Absent && !problem.isPersonMandatory(pid, currentTid)) {

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
  }

  // TODO From previous version:
  // - We might be examining swaps twice, once from each side. Can we exclude that?
  // - I might need to bring forbidden back as a constraint, to avoid evaluating the score when it's never gonna help.

}
