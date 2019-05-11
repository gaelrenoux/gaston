package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.{Problem, ScoredSchedule, Slot}
import fr.renoux.gaston.util.CollectionImplicits._
import fr.renoux.gaston.util.Tools

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random


/**
  * Improves an existing Schedule by moving persons around.
  */
class ScheduleImprover(val problem: Problem) {

  private val log = Logger[ScheduleImprover]

  /** Main method. Returns a schedule that's better than the initial one. Ends either because the schedule can't be
    * perfected any more or because the limit number of rounds has been reached. */
  def improve(scoredSchedule: ScoredSchedule, rounds: Int = 10000)(implicit rand: Random, tools: Tools): ScoredSchedule =
    tools.chrono("Improving persons") {
      log.debug("Improving new schedule")
      recImprove(scoredSchedule, rounds)
    }

  /** Recursive method improving the schedule. Works a bit on a slot before getting to the next one (slotRoundsLimit is
    * the parameter controlling how much we work on a single slot before going on, so that if we it the global limit we
    * had a good pass at all slots anyway). */
  @tailrec
  private def recImprove(
      scoredSchedule: ScoredSchedule,
      maxRounds: Int,
      slots: Queue[Slot] = Queue(problem.slots.toSeq: _*),
      slotRoundsLimit: Int = 1000
  )(implicit rand: Random): ScoredSchedule =
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
          recImprove(scoredSchedule, maxRounds - 1, slotsTail) //can't improve this slot any more

        case Some(candidate) =>
          /* The slot was perfected! If there are rounds left stay on the same slot, otherwise move to the next one */
          if (slotRoundsLimit > 0) recImprove(candidate, maxRounds - 1, slotsTail.enqueue(slot), slotRoundsLimit - 1)
          else recImprove(candidate, maxRounds - 1, slotsTail.enqueue(slot))
      }
    }

  /** Returns the first move or swap it finds that makes the schedule better, or None if there is nothing to do on that
    * slot anymore. */
  private def goodMoveOnSlot(scoredSchedule: ScoredSchedule, slot: Slot)(implicit rand: Random): Option[ScoredSchedule] = {
    val ScoredSchedule(schedule, score) = scoredSchedule
    val slotSchedule = schedule.on(slot)
    val topics = slotSchedule.topics

    val optionalOnTopic =
      topics.zipWith { t => schedule.personsPerTopic(t) -- problem.mandatoryPersonsPerTopic(t) }.toMap

    val topicsWithEnough = slotSchedule.records.filter { r => problem.minNumberPerTopic(r.topic) < r.persons.size }
    val topicsWithNotTooMuch = slotSchedule.records.filter { r => problem.maxNumberPerTopic(r.topic) > r.persons.size }

    /* All schedules on which we swapped two persons */
    val swappedSchedules = for {
      r1 <- rand.shuffle(slotSchedule.records.toSeq).view //TODO avoid duplicates (cases where we just swap r1 and r2)
      t1 = r1.topic
      r2 <- (slotSchedule.records - r1).view
      t2 = r2.topic
      p1 <- (optionalOnTopic(r1.topic) -- problem.forbiddenPersonsPerTopic(r2.topic)).view
      p2 <- (optionalOnTopic(r2.topic) -- problem.forbiddenPersonsPerTopic(r1.topic)).view
    } yield schedule.swapPersons(slot, (t1, p1), (t2, p2))

    /* All schedules on which we moved one person from one topic to another */
    val movedSchedules = for {
      r1 <- rand.shuffle(topicsWithEnough.toSeq).view //view to execute only while iterating, since we only want the first opportunity
      t1 = r1.topic
      r2 <- (topicsWithNotTooMuch - r1).view
      t2 = r2.topic
      p <- (optionalOnTopic(r1.topic) -- problem.forbiddenPersonsPerTopic(r2.topic)).view
    } yield schedule.movePerson(slot, t1, t2, p)

    val allNewSchedules = swappedSchedules ++ movedSchedules
    val scoredSchedules = allNewSchedules.zipWith(Scorer.score)
    scoredSchedules.dropWhile(_._2 <= score).headOption.map((ScoredSchedule.apply _).tupled)
  }

}