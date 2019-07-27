package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.Context._
import fr.renoux.gaston.model.{Problem, Schedule, Slot}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random


/**
  * Improves an existing Schedule by moving persons around. Does not reschedule topics, or remove them.
  */
class PersonPlacementImprover(implicit private val problem: Problem, private val ctx: Context) {

  private val log = Logger[PersonPlacementImprover]

  val defaultRoundsCount = 1000

  /** Main method. Returns a schedule that's better than the initial one. Ends either because the schedule can't be
    * perfected any more or because the limit number of rounds has been reached. */
  def improve(scoredSchedule: Schedule, rounds: Int = defaultRoundsCount)(implicit rand: Random): Schedule =
    chrono("PersonPlacementImprover >  improve") {
      log.trace("Improving persons")
      recImprove(scoredSchedule, rounds)
    }

  /** Recursive method improving the schedule. Works a bit on a slot before getting to the next one (slotRoundsLimit is
    * the parameter controlling how much we work on a single slot before going on, so that if we it the global limit we
    * had a good pass at all slots anyway). */
  @tailrec
  private def recImprove(
      scoredSchedule: Schedule,
      maxRounds: Int,
      slots: Queue[Slot] = Queue(problem.slotsSeq: _*),
      slotRoundsLimit: Int = 1000
  )(implicit rand: Random): Schedule =
    if (maxRounds == 0) {
      log.warn("Stopping improvement because max number of rounds was reached")
      scoredSchedule

    } else if (slots.isEmpty) {
      log.debug(s"Stopping improvement because all slots are perfect ($maxRounds rounds left)")
      scoredSchedule

    } else {
      val (slot, slotsTail) = slots.dequeue
      goodMoveOnSlot(scoredSchedule, slot) match {
        case None =>
          recImprove(scoredSchedule, maxRounds - 1, slotsTail) // can't improve this slot any more

        case Some(candidate) =>
          /* The slot was perfected! If there are rounds left stay on the same slot, otherwise move to the next one */
          if (slotRoundsLimit > 0) recImprove(candidate, maxRounds - 1, slotsTail.enqueue(slot), slotRoundsLimit - 1)
          else recImprove(candidate, maxRounds - 1, slotsTail.enqueue(slot))
      }
    }

  /** Returns the first move or swap it finds that makes the schedule better, or None if there is nothing to do on that
    * slot anymore. */
  private def goodMoveOnSlot(currentSchedule: Schedule, slot: Slot)(implicit rand: Random): Option[Schedule] =
    chrono("PersonPlacementImprover >  improve > goodMoveOnSlot") {
      if (false) rand.nextLong()
      val slotSchedule = currentSchedule.on(slot)

      lazy val records = rand.shuffle(slotSchedule.records)
      lazy val removablePersons = rand.shuffle(slotSchedule.records.filter(_.canRemovePersons))
      lazy val addablePersons = rand.shuffle(slotSchedule.records.filter(_.canAddPersons))

      /* All schedules on which we swapped two persons */
      lazy val swappedSchedules = for {
        r1 <- records.view
        r2 <- records.view if r1 < r2 // avoiding duplicates (cases where we just swap r1 and r2)
        t1 = r1.topic
        t2 = r2.topic
        p1 <- (r1.optionalPersons -- t2.forbidden).view
        p2 <- (r2.optionalPersons -- t1.forbidden).view
        improvedSchedule = currentSchedule.swapPersons(slot, (t1, p1), (t2, p2))
        if improvedSchedule.score > currentSchedule.score
      } yield improvedSchedule

      /* All schedules on which we moved one person from one topic to another */
      lazy val movedSchedules = for {
        r1 <- removablePersons.view
        r2 <- addablePersons.view if r1 != r2
        t1 = r1.topic
        t2 = r2.topic
        p <- (r1.optionalPersons -- t2.forbidden).view
        improvedSchedule = currentSchedule.movePerson(slot, t1, t2, p)
        if improvedSchedule.score > currentSchedule.score
      } yield improvedSchedule

      swappedSchedules.headOption orElse movedSchedules.headOption
    }

}
