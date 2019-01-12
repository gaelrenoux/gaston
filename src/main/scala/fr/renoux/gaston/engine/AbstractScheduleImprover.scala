package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.{Schedule, Score, Slot}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random

/** Base behavior of an improver. */
abstract class AbstractScheduleImprover extends ScheduleImprover {

  private val log = Logger(getClass)

  /** Main method. Returned an improved schedule, either because it can't be perfected any more or because the limit
    * number of rounds has been reached. */
  override def improve(schedule: Schedule, initialScore: Score, rounds: Int = 1000)(implicit rand: Random): Schedule =
    recImprove(schedule, initialScore, rounds)

  /** Recursive method improving the schedule. Works on slots in a round-robin. */
  @tailrec
  private def recImprove(
      schedule: Schedule,
      score: Score,
      maxRounds: Int,
      slots: Queue[Slot] = Queue(problem.slots.toSeq: _*)
  ): Schedule =
    if (maxRounds == 0) {
      log.debug("Stopping improvement because max number of rounds was reached")
      schedule

    } else if (slots.isEmpty) {
      log.debug(s"Stopping improvement because all slots are perfect ($maxRounds rounds left)")
      schedule

    } else {
      val (slot, slotsTail) = slots.dequeue
      val (candidate, candidateScore) = getMoveOnSlot(schedule, score, slot).getOrElse(schedule, score)
      if (candidateScore.value > score.value) {
        /* The slot was perfected, go on on the next slot and we'll go back to this one later */
        recImprove(candidate, candidateScore, maxRounds - 1, slotsTail.enqueue(slot))
      } else {
        /* The slot can't be perfected any more, go on to the next slot and no need to go back to this one */
        recImprove(schedule, score, maxRounds - 1, slotsTail)
      }
    }

  /** Returns the best possible move or swap on a specific slot, or None if there is nothing to do on that slot
    * anymore. Will be applied, so make sure you checked it was indeed better. */
  protected def getMoveOnSlot(schedule: Schedule, currentScore: Score, slot: Slot): Option[(Schedule, Score)]

}
