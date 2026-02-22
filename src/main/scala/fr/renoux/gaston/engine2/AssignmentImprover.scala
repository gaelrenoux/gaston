package fr.renoux.gaston.engine2

import fr.renoux.gaston.model2.*
import fr.renoux.gaston.util.Context.chrono
import fr.renoux.gaston.util.{Context, fastForeach, fastLoop}

import scala.math.Ordering.Implicits.infixOrderingOps
import scala.util.Random


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
  def improve(schedule: Schedule, maxRounds: Int = defaultMaxRoundsCount)(using Random): Unit =
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
  private def goodChangeForPerson(
      schedule: Schedule,
      assignment: SlotAssignment,
      topics: Array[TopicId],
      pid: PersonId
  )(using Random): Boolean = {
    val currentTid: TopicId = assignment.personsToTopic(pid)
    if (problem.isPersonMandatory(pid, currentTid)) {
      return false // Person is mandatory on the current topic, don't bother
    }

    topics.fastForeach { targetTid =>
      if (currentTid != targetTid) {
        if (goodChangeForPersonTopic(schedule, assignment, pid, currentTid, targetTid)) {
          return true
        }
      }
    }
    false
  }


  /** Find a good way to move that person, from that topic, to that topic. Returns true if it was done, false if there was no good way to do it.
    *
    * Assumption: the person is not mandatory on its current topic, and is not forbidden on the target topic. TODO: Add dev check for assumption
    */
  private def goodChangeForPersonTopic(
      schedule: Schedule,
      assignment: SlotAssignment,
      pid: PersonId, currentTid: TopicId, targetTid: TopicId
  )(using Random): Boolean = {
    /* The only iteration in this method is when we go over all persons on the target topic, when we try swaps */

    if (problem.isPersonForbidden(pid, targetTid)) {
      return false // Person is forbidden on the target topic, don't bother
    }

    val currentScore = schedule.getTotalScore
    schedule.saveScores()

    val currentLinkedTids = problem.topicsToLinkedTopics(currentTid)
    val currentLinkedTidsSize = currentLinkedTids.size
    val targetLinkedTids = problem.topicsToLinkedTopics(targetTid)
    val targetLinkedTidsSize = targetLinkedTids.size
    lazy val undoQueue = SmallUndoQueue(currentLinkedTidsSize.value + targetLinkedTidsSize.value)

    /* First, let's see if we can just move the person onto the target topic */
    if (assignment.isDroppableFromTopic(pid, currentTid) && assignment.isAddableToTopic(pid, targetTid)) {
      // TODO Assumption: if a person can be removed/added from a topic, it can be removed/added to all linked topics.
      //   This supposes that linked topics have the same min/max numbers, and the same mandatory persons.
      //   => Add check in the transcription, with reference to this method

      /* We still need to check that they can be added on the linked topics of the target topic, because they might be mandatory on their current topic there */
      val canMoveFromTopicOnLinkedSlots = targetLinkedTids.forall { targetLinkedTid =>
        val linkedSlotId = schedule.topicsToSlot(targetLinkedTid)
        val linkedAssignment = schedule.slotsToAssignment(linkedSlotId)
        val currentTidInLinkedSlot = linkedAssignment.personsToTopic(pid)
        linkedAssignment.isDroppableFromTopic(pid, currentTidInLinkedSlot)
      }

      if (canMoveFromTopicOnLinkedSlots) {

        /* We can now move that person on the target topic */
        assignment.move(pid, currentTid, targetTid)
        // note that we don't add this to the undoqueue, we can handle it manually

        /* Then we need to handle moving them out of the topics linked to the current one, and in the topics linked to the target one */
        undoQueue.reset()
        // TODO a lot of recalculation of stuff done in the check earlier, see if we can merge those steps
        currentLinkedTids.foreach { currentLinkedTid =>
          val linkedSlotId = schedule.topicsToSlot(currentLinkedTid)
          val linkedAssignment = schedule.slotsToAssignment(linkedSlotId)
          val linkedTidToTarget = linkedAssignment.pickGoodOpenTopicFor(pid)
          // We've already checked everything can be done, so no more checks before the move
          linkedAssignment.move(pid, currentLinkedTid, linkedTidToTarget)
          undoQueue.addMove(linkedSlotId, pid, currentLinkedTid, linkedTidToTarget)
        }
        targetLinkedTids.foreach { targetLinkedTid =>
          val linkedSlotId = schedule.topicsToSlot(targetLinkedTid)
          val linkedAssignment = schedule.slotsToAssignment(linkedSlotId)
          val currentTidInLinkedSlot = linkedAssignment.personsToTopic(pid)
          // We've already checked everything can be done, so no more checks before the move
          linkedAssignment.move(pid, currentTidInLinkedSlot, targetLinkedTid)
          undoQueue.addMove(linkedSlotId, pid, currentTidInLinkedSlot, targetLinkedTid)
        }

        /* Accept this change, if it looks good */
        schedule.recalculateScoreForPersonsPendingChanges()
        val newScore = schedule.getTotalScore
        if (newScore > currentScore) {
          return true
        }

        /* Wasn't good, rollback the changes */
        assignment.undoMove(pid, currentTid, targetTid)
        undoQueue.undoMoves { (sid, pid, tid1, tid2) =>
          val _ = schedule.slotsToAssignment(sid).undoMove(pid, tid1, tid2)
        }
        schedule.restoreSavedScores()
      }
    }

    /* If moving the person wasn't possible or didn't improve the score, we'll try to swap the person with another one on that topic */
    /* Method assumption is that the person is not mandatory on the source topic and not forbidden on the target topic, so we don't need to check for that */
    val targetTopicPersons = assignment.topicsToPersons(targetTid)
    // only examine persons that have a higher ID than the current one, to avoid looking at every swap twice (once from each side)
    targetTopicPersons.dropThenForeach(pid.value) { otherPid =>
      if (!problem.isPersonMandatory(otherPid, targetTid) && !problem.isPersonForbidden(otherPid, currentTid)) {

        /* We still need to check that the swap can happen on the linked topics (pid or otherPid might be mandatory/forbidden on the non-linked topics there) */
        lazy val canSwapOnCurrentLinkedSlots = currentLinkedTids.forall { currentLinkedTid =>
          val linkedSlotId = schedule.topicsToSlot(currentLinkedTid)
          val linkedAssignment = schedule.slotsToAssignment(linkedSlotId)
          val otherPersonThatTopicId = linkedAssignment.personsToTopic(otherPid)
          linkedAssignment.isAddableToTopic(pid, otherPersonThatTopicId) && linkedAssignment.isDroppableFromTopic(otherPid, otherPersonThatTopicId)
        }
        lazy val canSwapOnTargetLinkedSlots = targetLinkedTids.forall { targetLinkedTid =>
          val linkedSlotId = schedule.topicsToSlot(targetLinkedTid)
          val linkedAssignment = schedule.slotsToAssignment(linkedSlotId)
          val thatTopicId = linkedAssignment.personsToTopic(pid)
          linkedAssignment.isAddableToTopic(otherPid, thatTopicId) && linkedAssignment.isDroppableFromTopic(pid, thatTopicId)
        }

        if (canSwapOnCurrentLinkedSlots && canSwapOnTargetLinkedSlots) {

          /* We can now swap the two persons on the target topic */
          assignment.swap(pid, currentTid, otherPid, targetTid)
          // note that we don't add this to the undoqueue, we can handle it manually

          /* Then we need to handle the swap on the topics linked to the current one, and on the topics linked to the target one */
          undoQueue.reset()
          // TODO a lot of recalculation of stuff done in the check earlier, see if we can merge those steps
          currentLinkedTids.foreach { currentLinkedTid =>
            val linkedSlotId = schedule.topicsToSlot(currentLinkedTid)
            val linkedAssignment = schedule.slotsToAssignment(linkedSlotId)
            val otherPersonThatTopicId = linkedAssignment.personsToTopic(otherPid)
            // We've already checked everything can be done, so no more checks before the move
            linkedAssignment.swap(pid, currentLinkedTid, otherPid, otherPersonThatTopicId)
            undoQueue.addSwap(linkedSlotId, pid, currentLinkedTid, otherPid, otherPersonThatTopicId)
          }
          targetLinkedTids.foreach { targetLinkedTid =>
            val linkedSlotId = schedule.topicsToSlot(targetLinkedTid)
            val linkedAssignment = schedule.slotsToAssignment(linkedSlotId)
            val thatTopicId = linkedAssignment.personsToTopic(pid)
            // We've already checked everything can be done, so no more checks before the move
            linkedAssignment.swap(pid, thatTopicId, otherPid, targetLinkedTid)
            undoQueue.addSwap(linkedSlotId, pid, thatTopicId, otherPid, targetLinkedTid)
          }

          /* Accept this change, if it looks good */
          schedule.recalculateScoreForPersonsPendingChanges()
          val newScore = schedule.getTotalScore
          if (newScore > currentScore) {
            return true
          }

          /* Wasn't good, rollback the changes */
          val _ = assignment.undoSwap(pid, currentTid, otherPid, targetTid)
          undoQueue.undoSwaps { (sid, pid1, tid1, pid2, tid2) =>
            val _ = schedule.slotsToAssignment(sid).undoSwap(pid1, tid1, pid2, tid2)
          }
          schedule.restoreSavedScores()

        }
      }
    } // end dropThenForeach

    false
  }


  //  // TODO Unfinished and unused for now
  //  /** Find a good way to move that person on that topic. Returns true if it was done, false if there's no good way. */
  //  private def goodChangeForPersonTopicWithLinked(
  //      schedule: Schedule,
  //      assignment: SlotAssignment,
  //      pid: PersonId, currentTid: TopicId, targetTid: TopicId,
  //      handleLinked: Boolean
  //  ): Boolean = {
  //    /* The only iteration in this method is when we go over all persons on the target topic, when we try swaps */
  //
  //    if (problem.isPersonForbidden(pid, targetTid)) {
  //      return false // Person is forbidden on the target topic, don't bother
  //    }
  //
  //    val currentScore = schedule.getTotalScore
  //
  //    val currentLinkedTids = problem.topicsToLinkedTopics(currentTid)
  //    val targetLinkedTids = problem.topicsToLinkedTopics(targetTid)
  //
  //    /* First, let's see if we can just move the person onto the target topic */
  //    if (assignment.isDroppableFromTopic(pid, currentTid) && assignment.isAddableToTopic(pid, targetTid)) {
  //      /* We can just move that person on the target topic */
  //      assignment.move(pid, currentTid, targetTid)
  //      val newScore = schedule.getTotalScore
  //      if (newScore > currentScore) {
  //        return true // accept this change, it looks good
  //      }
  //      val _ = assignment.undoMove(pid, currentTid, targetTid) // wasn't good, rollback the change
  //    }
  //
  //    /* If moving the person wasn't possible or didn't improve the score, we'll try to swap the person with another one on that topic */
  //    val targetTopicPersons = assignment.topicsToPersons(targetTid)
  //    targetTopicPersons.foreach { otherPid =>
  //      // only examine persons that have a higher ID than the current one, to avoid looking at every swap twice (once from each side)
  //      if (pid.value < otherPid.value && !problem.isPersonMandatory(otherPid, targetTid) && !problem.isPersonForbidden(otherPid, currentTid)) {
  //        assignment.swap(pid, currentTid, otherPid, targetTid)
  //        val newScore = schedule.getTotalScore
  //        if (newScore > currentScore) {
  //          return true // accept this change, it looks good
  //        }
  //        val _ = assignment.undoSwap(pid, currentTid, otherPid, targetTid) // wasn't good, rollback the change
  //      }
  //    } // end foreachWhile
  //
  //    false
  //  }

}
