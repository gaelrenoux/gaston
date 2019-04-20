package fr.renoux.gaston.engine

import fr.renoux.gaston.model.{Problem, Schedule, Score, Slot}
import fr.renoux.gaston.util.CollectionImplicits._
import fr.renoux.gaston.util.Tools

/**
  * At each step, systematically iterate over all possible moves and swaps, and applies the first one that improves the
  * schedule.
  *
  * Way faster than [[ExhaustiveScheduleImprover]], but sometimes it doesn't get a result quite as good (very rarely, it
  * gets a better result, though).
  */
class GreedyScheduleImprover(val problem: Problem) extends ScheduleImprover.Base {

  /** Returns the first move or swap it finds that makes the schedule better */
  override protected def getMoveOnSlot(schedule: Schedule, currentScore: Score, slot: Slot)(implicit tools: Tools)
  : Option[(Schedule, Score)] = {
    val slotSchedule = schedule.on(slot)
    val topics = slotSchedule.topics

    val optionalOnTopic =
      topics.zipWith { t => schedule.personsPerTopic(t) -- problem.mandatoryPersonsPerTopic(t) }.toMap

    val topicsWithEnough = slotSchedule.records.filter { r => problem.minNumberPerTopic(r.topic) < r.persons.size }
    val topicsWithNotTooMuch = slotSchedule.records.filter { r => problem.maxNumberPerTopic(r.topic) > r.persons.size }

    /* All schedules on which we swapped two persons */
    val swappedSchedules = for {
      r1 <- slotSchedule.records.view //TODO use ordered records to avoid duplicates (cases where we just swap r1 and r2)
      t1 = r1.topic
      r2 <- (slotSchedule.records - r1).view
      t2 = r2.topic
      p1 <- (optionalOnTopic(r1.topic) -- problem.forbiddenPersonsPerTopic(r2.topic)).view
      p2 <- (optionalOnTopic(r2.topic) -- problem.forbiddenPersonsPerTopic(r1.topic)).view
    } yield schedule.swapPersons(slot, (t1, p1), (t2, p2))

    /* All schedules on which we moved one person from one topic to another */
    val movedSchedules = for {
      r1 <- topicsWithEnough.view //view to execute only while iterating, since we only want the first opportunity
      t1 = r1.topic
      r2 <- (topicsWithNotTooMuch - r1).view
      t2 = r2.topic
      p <- (optionalOnTopic(r1.topic) -- problem.forbiddenPersonsPerTopic(r2.topic)).view
    } yield schedule.movePerson(slot, t1, t2, p)

    val allNewSchedules = swappedSchedules ++ movedSchedules
    val scoredSchedules = allNewSchedules.zipWith(Scorer.score)
    scoredSchedules.dropWhile(_._2 <= currentScore).headOption
  }

}
