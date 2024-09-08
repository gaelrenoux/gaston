package fr.renoux.gaston.engine.assignment

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model._
import fr.renoux.gaston.util.Context
import fr.renoux.gaston.util.Context.chrono

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random


/**
  * Improves an existing Schedule by moving persons around. Does not reschedule topics, or remove them. Every person
  * should already be assigned somewhere (this will not assign unassigned persons).
  */
final class AssignmentImprover(implicit private val problem: Problem, private val ctx: Context) {

  private val log = Logger[AssignmentImprover]

  private val defaultMaxRoundsCount = 1000

  /** This value is to avoid one slot hugging all round to improve itself, leaving no rounds to the other slots. */
  private val defaultMaxSuccessiveRoundsCountPerSlot = defaultMaxRoundsCount / problem.slotsSet.size

  /** Main method. Returns a schedule that's better than the initial one. Ends either because the schedule can't be
    * perfected any more or because the limit number of rounds has been reached. */
  def improve(schedule: Schedule, maxRounds: Int = defaultMaxRoundsCount)(implicit rand: Random): Schedule =
    chrono("PersonPlacementImprover >  improve") {
      recImprove(schedule, maxRounds)
    }

  /** Recursive method improving the schedule. Works a bit on a slot before getting to the next one (slotRoundsLimit is
    * the parameter controlling how much we work on a single slot before going on, so that if we hit the global limit we
    * had a good pass at all slots anyway). */
  @tailrec
  private def recImprove(
      schedule: Schedule,
      maxRounds: Int,
      slots: Queue[Slot] = Queue(problem.slotsList: _*),
      slotRoundsLimit: Int = defaultMaxSuccessiveRoundsCountPerSlot
  )(implicit rand: Random): Schedule =
    if (maxRounds == 0) {
      log.warn(s"Stopping assignment improvement because max number of rounds was reached. Slots left to optimize are: ${slots.view.map(_.name).toList}.")
      schedule

    } else if (slots.isEmpty) {
      log.debug(s"Stopping assignment improvement because all slots are perfect ($maxRounds rounds left)")
      schedule

    } else {
      val (slot, slotsTail) = slots.dequeue

      goodMoveOnSlot(schedule, slot) match {
        case None =>
          /* can't improve this slot any more ! Continue on the slots left */
          recImprove(schedule, maxRounds - 1, slotsTail)

        case Some(candidate) =>
          /* The slot was improved! If there are rounds left stay on the same slot, otherwise move to the next one and come back to this one later. */
          if (slotRoundsLimit > 0) recImprove(candidate, maxRounds - 1, slots, slotRoundsLimit - 1) // current slot still on top of the queue
          else recImprove(candidate, maxRounds - 1, slotsTail.enqueue(slot)) // current slot moved to the bottom of the queue
      }
    }

  /** Returns the first move or swap it finds that makes the schedule better, or None if there is nothing to do on that
    * slot anymore. */
  private def goodMoveOnSlot(currentSchedule: Schedule, slot: Slot)(implicit rand: Random): Option[Schedule] =
    chrono("PersonPlacementImprover >  improve > goodMoveOnSlot") {
      val slotSchedule = currentSchedule.on(slot)

      lazy val records: Iterable[Record] = rand.shuffle(slotSchedule.records)
      lazy val recordsRemovable: Iterable[Record] = rand.shuffle(slotSchedule.recordsThatCanRemovePersons)
      lazy val recordsAddable: Iterable[Record] = rand.shuffle(slotSchedule.recordsThatCanAddPersons)

      /* All schedules on which we swapped two persons */
      lazy val swappedSchedules = for {
        r1 <- records.view
        r2 <- records.view if r1 < r2 // avoiding duplicates (cases where we just swap r1 and r2)
        t1 = r1.topic
        t2 = r2.topic
        p1 <- (r1.optionalPersons -- t2.forbidden).view
        p2 <- (r2.optionalPersons -- t1.forbidden).view
        scoreImprovement = currentSchedule.deltaScoreIfSwapPerson(slot, (t1, p1), (t2, p2))
        if scoreImprovement.forall(_.isPositive) // if we couldn't calculate one, we have to continue and check the improved schedule
        improvedSchedule = currentSchedule.swapPersons(slot, (t1, p1), (t2, p2))
        if improvedSchedule.score > currentSchedule.score // second check is negligible in performance, we need the improvedSchedule's score anyway
      } yield improvedSchedule

      /* All schedules on which we moved one person from one topic to another */
      lazy val movedSchedules = for {
        r1 <- recordsRemovable.view
        r2 <- recordsAddable.view if r1 != r2
        t1 = r1.topic
        t2 = r2.topic
        p <- (r1.optionalPersons -- t2.forbidden).view
        improvedSchedule = currentSchedule.movePerson(slot, t1, t2, p)
        if improvedSchedule.score > currentSchedule.score
      } yield improvedSchedule

      swappedSchedules.headOption orElse movedSchedules.headOption
    }

}
