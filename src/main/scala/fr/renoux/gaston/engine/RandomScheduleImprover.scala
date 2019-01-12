package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Schedule, Score}
import fr.renoux.gaston.util.RandomImplicits._

import scala.annotation.tailrec
import scala.util.Random

/**
  * Improves an existing Schedule by satisfying preferences.
  */
class RandomScheduleImprover(val problem: Problem, val scorer: Scorer) extends ScheduleImprover {

  private val log = Logger[RandomScheduleImprover]

  override def improve(schedule: Schedule, initialScore: Score, rounds: Int)(implicit rand: Random): Schedule =
    simpleRandomizedAmelioration(schedule, initialScore, rounds)

  /** Use random swaps trying to always improve the solution. Pretty fast, but no garantee to have a perfect solution. */
  @tailrec
  private def simpleRandomizedAmelioration(schedule: Schedule, previousScore: Score, rounds: Int = 10000)(implicit rand: Random): Schedule = if (rounds == 0) schedule else {

    val candidate = randomSwap(schedule)
    val candidateScore = scorer.score(candidate)

    if (candidateScore.value >= previousScore.value) simpleRandomizedAmelioration(candidate, candidateScore, rounds - 1)
    else simpleRandomizedAmelioration(schedule, previousScore, rounds - 1)
  }

  @tailrec
  private def randomSwap(schedule: Schedule, attempts: Int = 1000)(implicit rand: Random): Schedule = {
    if (attempts < 0) throw new IllegalArgumentException(s"$schedule")

    val suitableSlots = schedule.topicsPerSlot.filter(_._2.size > 1).keySet
    if (suitableSlots.isEmpty) throw new IllegalArgumentException("Can't swap anything...")
    val slot = rand.pick(suitableSlots)
    val picks = rand.pick(schedule.records filter (_.slot == slot), 2)
    val r1 = picks(0)
    val r2 = picks(1)
    /* we have picked two records on the same random slot */

    val mandatoryPersonsR1 = problem.mandatoryPersonsPerTopic(r1.topic)
    val mandatoryPersonsR2 = problem.mandatoryPersonsPerTopic(r2.topic)
    val forbiddenPersonsR1 = problem.forbiddenPersonsPerTopic(r1.topic)
    val forbiddenPersonsR2 = problem.forbiddenPersonsPerTopic(r2.topic)

    val acceptableT1Picks = r1.persons -- mandatoryPersonsR1 -- forbiddenPersonsR2
    val acceptableT2Picks = r2.persons -- mandatoryPersonsR2 -- forbiddenPersonsR1

    if (acceptableT1Picks.isEmpty || acceptableT2Picks.isEmpty) randomSwap(schedule, attempts - 1)
    else {
      val p1 = rand.pick(acceptableT1Picks)
      val p2 = rand.pick(acceptableT2Picks)

      val newR1 = r1.copy(persons = r1.persons - p1 + p2)
      val newR2 = r2.copy(persons = r2.persons - p2 + p1)

      log.trace(s"Swapping ${p1.name} and ${p2.name} on slot ${slot.name}")
      schedule.copy(records = schedule.records - r1 - r2 + newR1 + newR2)
    }
  }


}
