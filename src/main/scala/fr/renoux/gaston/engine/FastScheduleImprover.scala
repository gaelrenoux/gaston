package fr.renoux.gaston.engine

import fr.renoux.gaston.model.{Problem, Schedule, Score, Slot}
import fr.renoux.gaston.util.CollectionImplicits._

/**
  * Improves an existing Schedule by satisfying preferences. At each step, explores systematically all possible moves
  * and swaps, and applies the first one that improves the schedule. Faster to converge to an optimum than
  * SystematicScheduleImprover, but sometimes it doesn't get a result quite as good. Very rarely, gets a better result,
  * though.
  */
class FastScheduleImprover(val problem: Problem) extends AbstractScheduleImprover {

  /** Returns the first move or swap it finds that makes the schedule better */
  override protected def getMoveOnSlot(schedule: Schedule, currentScore: Score, slot: Slot): Option[(Schedule, Score)
    ] = {
    val topics = schedule.topicsPerSlot(slot)
    val records = schedule.onSlot(slot)

    val movableFromTopic =
      topics.zipWith { t => schedule.personsPerTopic(t) -- problem.mandatoryPersonsPerTopic(t) }.toMap

    /* All schedules on which we swapped two persons */
    val swappedSchedules = for {
      r1 <- records.view
      r2 <- (records - r1).view
      p1 <- movableFromTopic(r1.topic) -- problem.forbiddenPersonsPerTopic(r2.topic)
      p2 <- movableFromTopic(r2.topic) -- problem.forbiddenPersonsPerTopic(r1.topic)
    } yield {
      val newR1 = r1.copy(persons = r1.persons - p1 + p2)
      val newR2 = r2.copy(persons = r2.persons - p2 + p1)
      schedule.updateRecords(_ - r1 - r2 + newR1 + newR2)
    }

    /* All schedules on which we moved one person from one topic to another */
    val movedSchedules = for {
      r1 <- records.view.filter { r => problem.minNumberPerTopic.getOrElse(r.topic, 0) < r.persons.size }
      r2 <- (records - r1).view
        .filter { r => problem.maxNumberPerTopic.getOrElse(r.topic, Int.MaxValue) > r.persons.size }
      p1 <- movableFromTopic(r1.topic) -- problem.forbiddenPersonsPerTopic(r2.topic)
    } yield {
      val newR1 = r1.copy(persons = r1.persons - p1)
      val newR2 = r2.copy(persons = r2.persons + p1)
      schedule.updateRecords(_ - r1 - r2 + newR1 + newR2)
    }

    val allNewSchedules = swappedSchedules ++ movedSchedules
    val scoredSchedules = allNewSchedules.zipWith(Scorer.score)
    scoredSchedules.dropWhile(_._2 <= currentScore).headOption
  }

}
