package fr.renoux.gaston.engine2

import fr.renoux.gaston.model2.*
import fr.renoux.gaston.util.Context.chrono
import fr.renoux.gaston.util.{Context, fastForeach, fastLoop}

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
          val foundGoodChange = goodChangeForPerson(schedule, assignment, topics, pid)
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


  /** Iterate over all possible topics for a move on that slot, for that person */
  private def goodChangeForPerson(schedule: Schedule, assignment: SlotAssignment, topics: Array[TopicId], pid: PersonId): Boolean = {

    val currentTid: TopicId = assignment.personsToTopic(pid)
    val currentTidLinks = schedule.problem.prefsTopicsLinked

    topics.fastForeach { targetTid =>
      if (currentTid != targetTid) {
        if (goodChangeForPersonTopic(schedule, assignment, pid, currentTid, targetTid)) {
          return true
        }
      }
    }
    false
  }


  /** Find a good way to move that person on that topic. Returns true if it was done, false if there's no good way. */
  private def goodChangeForPersonTopic(schedule: Schedule, assignment: SlotAssignment, pid: PersonId, currentTid: TopicId, targetTid: TopicId): Boolean = {
    /* The only iteration in this method is when we go over all persons on the target topic, when we try swaps */

    if (problem.isPersonForbidden(pid, targetTid)) {
      return false // Person is forbidden on the target topic, don't bother
    }

    val currentScore = schedule.getTotalScore()
    /* First, let's see if we can just move the person onto the target topic */
    if (assignment.isDroppableFromTopic(pid, currentTid) && assignment.isAddableToTopic(pid, targetTid)) {
      /* We can just move that person on the target topic */
      assignment.move(pid, currentTid, targetTid)
      val newScore = schedule.getTotalScore()
      if (newScore > currentScore) {
        return true // accept this change, it looks good
      }
      val _ = assignment.undoMove(pid, currentTid, targetTid) // wasn't good, rollback the change
    }

    /* If moving the person wasn't possible or didn't improve the score, we'll try to swap the person with another one on that topic */
    val targetTopicPersons = assignment.topicsToPersons(targetTid)
    targetTopicPersons.foreach { otherPid =>
      // only examine persons that have a higher ID than the current one, to avoid looking at every swap twice (once from each side)
      if (pid.value < otherPid.value && !problem.isPersonMandatory(otherPid, targetTid) && !problem.isPersonForbidden(otherPid, currentTid)) {
        assignment.swap(pid, currentTid, otherPid, targetTid)
        val newScore = schedule.getTotalScore()
        if (newScore > currentScore) {
          return true // accept this change, it looks good
        }
        val _ = assignment.undoSwap(pid, currentTid, otherPid, targetTid) // wasn't good, rollback the change
      }
    } // end foreachWhile

    false
  }

  // TODO From previous version:
  // - Need to handle linked topic. Right now we just can't move people on/off them.

}
