package fr.renoux.gaston.engine.assignment

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model._
import fr.renoux.gaston.util.Context
import fr.renoux.gaston.util.Context.chrono

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.util.Random


/**
  * Improves an existing Schedule by moving persons around. Does not reschedule topics, or remove them.
  */
final class AssignmentImprover(implicit private val problem: Problem, private val ctx: Context) {

  private val cache: mutable.Map[Schedule.Planning, Schedule] = TrieMap[Schedule.Planning, Schedule]()

  private val slotCache: mutable.Map[Set[Topic], SlotSchedule] = TrieMap[Set[Topic], SlotSchedule]()

  private val log = Logger[AssignmentImprover]

  val defaultRoundsCount = 100000

  /** Main method. Returns a schedule that's better than the initial one. Ends either because the schedule can't be
    * perfected any more or because the limit number of rounds has been reached. */
  def improve(scoredSchedule: Schedule, rounds: Int = defaultRoundsCount)(implicit rand: Random): Schedule =
    chrono("PersonPlacementImprover >  improve") {
      cache.getOrElseUpdate(scoredSchedule.planning, recImprove(scoredSchedule, rounds))
    }

  /** Recursive method improving the schedule. Works a bit on a slot before getting to the next one (slotRoundsLimit is
    * the parameter controlling how much we work on a single slot before going on, so that if we hit the global limit we
    * had a good pass at all slots anyway). */
  @tailrec
  private def recImprove(
      schedule: Schedule,
      maxRounds: Int,
      slots: Queue[Slot] = Queue(problem.slotsList: _*),
      slotRoundsLimit: Int = 1000
  )(implicit rand: Random): Schedule =
    if (maxRounds == 0) {
      log.warn("Stopping assignment improvement because max number of rounds was reached")
      schedule

    } else if (slots.isEmpty) {
      log.debug(s"Stopping improvement because all slots are perfect ($maxRounds rounds left)")
      schedule

    } else {
      val (slot, slotsTail) = slots.dequeue
      val slotSchedule = schedule.on(slot)

      slotCache.get(slotSchedule.topicsSet) match {
        case Some(ss) =>
          recImprove(schedule.set(ss), maxRounds - 1, slotsTail) // slot read from the cache, go to the next one

        case None => goodMoveOnSlot(schedule, slot) match {

          case None =>
            /* can't improve this slot any more ! Store in cache, then go to next slot */
            slotCache.update(slotSchedule.topicsSet, slotSchedule)
            recImprove(schedule, maxRounds - 1, slotsTail)

          case Some(candidate) =>
            /* The slot was perfected! If there are rounds left stay on the same slot, otherwise move to the next one */
            if (slotRoundsLimit > 0) recImprove(candidate, maxRounds - 1, slotsTail.enqueue(slot), slotRoundsLimit - 1)
            else recImprove(candidate, maxRounds - 1, slotsTail.enqueue(slot))
        }
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
        if !t1.forced // TODO UGLYYYYYYY
        if !t2.forced // TODO UGLYYYYYYY
        p1 <- (r1.optionalPersons -- t2.forbidden).view
        p2 <- (r2.optionalPersons -- t1.forbidden).view
        scoreImprovement = currentSchedule.deltaScoreIfSwapPerson(slot, (t1, p1), (t2, p2))
        if scoreImprovement.value > 0
        /*_ = {
          if (math.abs(improvedSchedule.score.value - currentSchedule.score.value - scoreImprovement.value) < 0.01) ()
          else throw new IllegalStateException(s"${improvedSchedule.score} - ${currentSchedule.score} <> $scoreImprovement")
        }*/
      } yield currentSchedule.swapPersons(slot, (t1, p1), (t2, p2))

      /* All schedules on which we moved one person from one topic to another */
      lazy val movedSchedules = for {
        r1 <- recordsRemovable.view
        r2 <- recordsAddable.view if r1 != r2
        t1 = r1.topic
        t2 = r2.topic
        if !t1.forced // TODO UGLYYYYYYY
        if !t2.forced // TODO UGLYYYYYYY
        p <- (r1.optionalPersons -- t2.forbidden).view
        improvedSchedule = currentSchedule.movePerson(slot, t1, t2, p)
        if improvedSchedule.score > currentSchedule.score
      } yield improvedSchedule

      swappedSchedules.headOption orElse movedSchedules.headOption
    }

}
