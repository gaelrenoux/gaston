package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Schedule, Score, Slot}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random

/**
  * Improves an existing Schedule by satisfying preferences.
  */
class SystematicScheduleImprover(problem: Problem) extends ScheduleImprover(problem) {

  private val log = Logger[SystematicScheduleImprover]

  override def improve(schedule: Schedule, initialScore: Score, rounds: Int = 1000)(implicit rand: Random): Schedule =
    systematicAmelioration(schedule, initialScore, rounds)

  /** Systematically explores all swaps. Slower than the randomized method. */
  @tailrec
  private def systematicAmelioration(schedule: Schedule, score: Score, maxRounds: Int = 1000, slots: Queue[Slot] = Queue(problem.slots.toSeq: _*)): Schedule =
  if (maxRounds == 0) {
    log.debug("Stopping systematic amelioration because max number of rounds was reached")
    schedule
  } else if (slots.isEmpty) {
    log.debug(s"Stopping systematic amelioration because all slots are perfect ($maxRounds rounds left)")
    schedule
  } else {
    val (slot, slotsTail) = slots.dequeue
    val (candidate, candidateScore) = bestMoveOnSlot(schedule, slot).getOrElse(schedule, score)
    if (candidateScore.value > score.value)
      systematicAmelioration(candidate, candidateScore, maxRounds - 1, slotsTail.enqueue(slot))
    else
      systematicAmelioration(schedule, score, maxRounds - 1, slotsTail)
  }

  /** Returns the best possible move or swap on a specific slot */
  private def bestMoveOnSlot(schedule: Schedule, slot: Slot): Option[(Schedule, Score)] = {
    val topics = schedule.topicsPerSlot(slot)
    val records = schedule.records.filter(_.slot == slot)

    val movableFromTopic = topics map { t => t -> (schedule.personsPerTopic(t) -- problem.mandatoryPersonsPerTopic(t)) } toMap

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

    val movedSchedules = for {
      r1 <- records filter { r => problem.minNumberPerTopic.getOrElse(r.topic, 0) < r.persons.size }
      r2 <- (records - r1) filter { r => problem.maxNumberPerTopic.getOrElse(r.topic, Int.MaxValue) > r.persons.size }
      p1 <- movableFromTopic(r1.topic) -- problem.forbiddenPersonsPerTopic(r2.topic)
    } yield {
      val newR1 = r1.copy(persons = r1.persons - p1)
      val newR2 = r2.copy(persons = r2.persons + p1)
      schedule.copy(records = schedule.records - r1 - r2 + newR1 + newR2)
    }

    val allNewSchedules = swappedSchedules ++ movedSchedules

    if (swappedSchedules.nonEmpty) Some(swappedSchedules map { s => (s, score(s)) } maxBy (_._2))
    else None
  }

}
