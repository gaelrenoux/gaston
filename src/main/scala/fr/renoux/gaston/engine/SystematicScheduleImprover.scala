package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Schedule, Score, Slot}
import fr.renoux.gaston.util.CollectionImplicits._

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random

/**
  * Improves an existing Schedule by satisfying preferences.
  */
class SystematicScheduleImprover(val problem: Problem, val scorer: Scorer) extends ScheduleImprover {

  private val log = Logger[SystematicScheduleImprover]

  /** Main method. Returned un improved schedule, either because it can't be perfected any more or because the limit
    * number of rounds has been reached. */
  override def improve(schedule: Schedule, initialScore: Score, rounds: Int = 1000)(implicit rand: Random): Schedule =
    systematicAmelioration(schedule, initialScore, rounds)

  /** Systematically explores all swaps. Slower than the randomized method. */
  @tailrec
  private def systematicAmelioration(schedule: Schedule, score: Score, maxRounds: Int = 1000, slots: Queue[Slot] =
  Queue(problem
    .slots.toSeq: _*)): Schedule =
    if (maxRounds == 0) {
      log.debug("Stopping systematic amelioration because max number of rounds was reached")
      schedule
    } else if (slots.isEmpty) {
      log.debug(s"Stopping systematic amelioration because all slots are perfect ($maxRounds rounds left)")
      schedule
    } else {
      val (slot, slotsTail) = slots.dequeue
      val (candidate, candidateScore) = bestMoveOnSlot(schedule, slot).getOrElse(schedule, score)
      if (candidateScore.value > score.value) {
        /* The slot was perfected, go on on the next slot and we'll go back to this one later */
        systematicAmelioration(candidate, candidateScore, maxRounds - 1, slotsTail.enqueue(slot))
      } else {
        /* The slot can't be perfected any more, go on to the next slot and no need to go back to this one */
        systematicAmelioration(schedule, score, maxRounds - 1, slotsTail)
      }
    }

  /** Returns the best possible move or swap on a specific slot */
  private def bestMoveOnSlot(schedule: Schedule, slot: Slot): Option[(Schedule, Score)] = {
    val topics = schedule.topicsPerSlot(slot)
    val records = schedule.records.filter(_.slot == slot)

    val movableFromTopic =
      topics.zipWith { t => schedule.personsPerTopic(t) -- problem.mandatoryPersonsPerTopic(t) }.toMap

    /* All schedules on which we swapped two persons */
    val swappedSchedules = for {
      r1 <- records
      r2 <- records - r1
      p1 <- movableFromTopic(r1.topic) -- problem.forbiddenPersonsPerTopic(r2.topic)
      p2 <- movableFromTopic(r2.topic) -- problem.forbiddenPersonsPerTopic(r1.topic)
    } yield {
      val newR1 = r1.copy(persons = r1.persons - p1 + p2)
      val newR2 = r2.copy(persons = r2.persons - p2 + p1)
      schedule.copy(records = schedule.records - r1 - r2 + newR1 + newR2)
    }

    /* All schedules on which we moved one person from one topic to another */
    val movedSchedules = for {
      r1 <- records.filter { r => problem.minNumberPerTopic.getOrElse(r.topic, 0) < r.persons.size }
      r2 <- (records - r1).filter { r => problem.maxNumberPerTopic.getOrElse(r.topic, Int.MaxValue) > r.persons.size }
      p1 <- movableFromTopic(r1.topic) -- problem.forbiddenPersonsPerTopic(r2.topic)
    } yield {
      val newR1 = r1.copy(persons = r1.persons - p1)
      val newR2 = r2.copy(persons = r2.persons + p1)
      schedule.copy(records = schedule.records - r1 - r2 + newR1 + newR2)
    }

    val allNewSchedules = swappedSchedules ++ movedSchedules
    if (allNewSchedules.nonEmpty) Some(allNewSchedules.zipWith(scorer.score).maxBy(_._2))
    else None
  }

}
