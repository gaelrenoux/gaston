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
  override protected def getMoveOnSlot(schedule: Schedule, currentScore: Score, slot: Slot)
  : Option[(Schedule, Score)] = {
    val slotSchedule = schedule.on(slot)
    val topics = slotSchedule.topics

    val optionalOnTopic =
      topics.zipWith { t => schedule.personsPerTopic(t) -- problem.mandatoryPersonsPerTopic(t) }.toMap

    val topicsWithEnough = slotSchedule.records.filter { r => problem.minNumberPerTopic(r.topic) < r.persons.size }
    val topicsWithNotTooMuch = slotSchedule.records.filter { r => problem.maxNumberPerTopic(r.topic) > r.persons.size }

    /* All schedules on which we swapped two persons */
    val swappedSchedules = for {
      r1 <- slotSchedule.records
      t1 = r1.topic
      r2 <- slotSchedule.records - r1
      t2 = r2.topic
      p1 <- optionalOnTopic(r1.topic) -- problem.forbiddenPersonsPerTopic(r2.topic)
      p2 <- optionalOnTopic(r2.topic) -- problem.forbiddenPersonsPerTopic(r1.topic)
    } yield schedule.swapPersons(slot, (t1, p1), (t2, p2))

    /* All schedules on which we moved one person from one topic to another */
    val movedSchedules = for {
      r1 <- topicsWithEnough
      t1 = r1.topic
      r2 <- topicsWithNotTooMuch - r1
      t2 = r2.topic
      p <- optionalOnTopic(r1.topic) -- problem.forbiddenPersonsPerTopic(r2.topic)
    } yield schedule.movePerson(slot, t1, t2, p)

    val allNewSchedules = swappedSchedules ++ movedSchedules
    val scoredSchedules = allNewSchedules.zipWith(Scorer.score)

    if (scoredSchedules.isEmpty) None else {
      val candidateAndScore = scoredSchedules.maxBy(_._2)
      if (candidateAndScore._2 > currentScore) Some(candidateAndScore) else None
    }
  }

}
