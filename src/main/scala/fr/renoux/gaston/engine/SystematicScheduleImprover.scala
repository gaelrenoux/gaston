package fr.renoux.gaston.engine

import fr.renoux.gaston.model.{Problem, Schedule, Score, Slot}
import fr.renoux.gaston.util.CollectionImplicits._

/**
  * Improves an existing Schedule by satisfying preferences. At each step, explores systematically all possible moves
  * and swaps, and applies the best one. Only works on two-person swaps. It is slow, as the same move will be examined
  * many times.
  */
class SystematicScheduleImprover(val problem: Problem) extends AbstractScheduleImprover {

  /** Returns the best possible move or swap on a specific slot */
  override protected def getMoveOnSlot(schedule: Schedule, currentScore: Score, slot: Slot): Option[(Schedule, Score)
    ] = {
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
    val scoredSchedules = allNewSchedules.zipWith(Scorer.score(problem, _))

    if (scoredSchedules.isEmpty) None else {
      val candidateAndScore = scoredSchedules.maxBy(_._2)
      if (candidateAndScore._2 > currentScore) Some(candidateAndScore) else None
    }
  }

}

