package fr.renoux.gaston.engine2

import fr.renoux.gaston.model2.*
import fr.renoux.gaston.util.Context
import fr.renoux.gaston.util.Context.chrono

import scala.math.Ordering.Implicits.infixOrderingOps


/** Improves an existing Schedule by moving persons around. Does not reschedule topics, or remove them. Every person
  * should already be assigned somewhere (this will not assign unassigned persons).
  */
final class AssignmentImprover(private val problem: SmallProblem)(using private val ctx: Context) {

  given SmallProblem = problem

  import problem.given

  private val defaultMaxRoundsCount = 1000

  /** This value is to avoid one slot hugging all round to improve itself, leaving no rounds to the other slots. */
  private val defaultMaxSuccessiveRoundsCountPerSlot = defaultMaxRoundsCount / problem.slotsCount.value

  /** Main method. Modifies the schedule to make it better (mutates the argument).
    *
    * Ends either because the schedule can't be perfected any more or because the limit number of rounds has been
    * reached.
    */
  def improve(schedule: Schedule, maxRounds: Int = defaultMaxRoundsCount): Unit =
    chrono("PersonPlacementImprover >  improve") {

      var roundsLeft = maxRounds
      var sid: SlotId = 0 // start on the first slot
      var blockedSlots = 0

      /* Iterate over slots */
      while (blockedSlots < problem.slotsCount && roundsLeft > 0) {
        val persons = schedule.slotsToAssignment(sid).mobilePersons.toArray
        val topics = schedule.slotsToTopics(sid).toArray
        val assignment = schedule.slotsToAssignment(sid)

        var changesMade = false
        var subRoundsLeft = math.min(roundsLeft, defaultMaxSuccessiveRoundsCountPerSlot)
        var personIx = 0
        var blockedPersons = 0

        /* Iterate over persons to change */
        while (blockedPersons < persons.length && subRoundsLeft > 0) {
          val pid = persons(personIx)
          var targetTopicIx = 0
          var foundGoodChange = false

          /* Iterate over target topics for a move */
          while (!foundGoodChange && targetTopicIx < topics.length) {
            val targetTid = topics(targetTopicIx)
            foundGoodChange = goodChangeForPersonTopic(schedule, assignment, pid, targetTid)
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

          personIx = (personIx + 1) % persons.length
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


  /** Find a good way to move that person on that topic. Returns true if it was done, false if there's no good way. */
  private def goodChangeForPersonTopic(schedule: Schedule, assignment: SlotAssignment, pid: PersonId, targetTid: TopicId): Boolean = {
    /* The only iteration in this method is when we go over all persons on the target topic, when we try swaps */

    if (problem.isPersonForbidden(pid, targetTid)) false // Person is forbidden on the target topic, don't bother
    else {
      val currentTid = assignment.personsToTopic(pid)
      if (currentTid == targetTid) false
      else {
        var found = false
        val currentScore = schedule.getTotalScore()

        /* First, let's see if we can just move the person onto the target topic */
        if (assignment.isDroppableFromTopic(pid, currentTid) && assignment.isAddableToTopic(pid, targetTid)) {
          /* We can just move that person on the target topic */
          assignment.move(pid, currentTid, targetTid)
          val newScore = schedule.getTotalScore()
          if (newScore <= currentScore) {
            val _ = assignment.undoMove(pid, currentTid, targetTid)
          } else {
            // found a good one
            found = true
          }
        }

        /* If moving the person wasn't possible or didn't improve the score, we'll try to swap the person with another one on that topic */
        if (!found) {
          val targetTopicPersons = assignment.topicsToPersons(targetTid)
          targetTopicPersons.foreachWhile { otherPid =>
            // only examine persons that have a higher ID than the current one, to avoid looking at every swap twice (once from each side)
            if (pid.value < otherPid.value && !problem.isPersonMandatory(otherPid, targetTid) && !problem.isPersonForbidden(otherPid, currentTid)) {
              assignment.swap(pid, currentTid, otherPid, targetTid)
              val newScore = schedule.getTotalScore()
              if (newScore <= currentScore) {
                val _ = assignment.undoSwap(pid, currentTid, otherPid, targetTid)
              } else {
                // found a good one
                found = true
              }
            }
            !found
          } // end foreachWhile
        }

        found
      }
    }
  }

  // TODO From previous version:
  // - Need to handle linked topic. Right now we just can't move people on/off them.

}
