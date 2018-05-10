package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Schedule, Score, Slot}
import fr.renoux.gaston.util.RandomImplicits._

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random

/**
  * Improves an existing Schedule by satisfying preferences.
  */
class PreferredScheduleFactory(problem: Problem) extends Scoring(problem) {

  private val log = Logger[PreferredScheduleFactory]

  /** Use random swaps trying to always improve the solution. Pretty fast, but no garantee to have a perfect solution. */
  @tailrec
  final def simpleRandomizedAmelioration(schedule: Schedule, previousScore: Score, rounds: Int = 10000)(implicit rand: Random): Schedule = if (rounds == 0) schedule else {

    val candidate = randomSwap(schedule)
    val candidateScore = score(candidate)

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

  /** Systematically explores all swaps. Not perfect because it only swaps, it does not explore more. Slower than the randomized method. */
  @tailrec
  final def systematicAmelioration(schedule: Schedule, score: Score, maxRounds: Int = 1000, slots: Queue[Slot] = Queue(problem.slots.toSeq: _*)): Schedule =
  if (maxRounds == 0) {
    log.debug("Stopping systematic amelioration because max number of rounds was reached")
    schedule
  } else if (slots.isEmpty) {
    log.debug("Stopping systematic amelioration because all slots are perfect")
    schedule
  } else {
    val (slot, slotsTail) = slots.dequeue
    val (candidate, candidateScore) = bestSwapOnSlot(schedule, slot).getOrElse(schedule, score)
    if (candidateScore.value > score.value)
      systematicAmelioration(candidate, candidateScore, maxRounds - 1, slotsTail.enqueue(slot))
    else
      systematicAmelioration(schedule, score, maxRounds - 1, slotsTail)
  }

  /** Returns the best possible swap on a specific slot */
  private def bestSwapOnSlot(schedule: Schedule, slot: Slot): Option[(Schedule, Score)] = {
    //TODO fint best swap OR simple move !! Sometimes a simple move is possible and better...
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
      /* if (p1 == Person("fatima") && p2 == Person("adam")) Some(schedule.copy(schedule.records - r1 - r2 + newR1 + newR2))
      else None */
    }

    if (swappedSchedules.nonEmpty) Some(swappedSchedules map { s => (s, score(s)) } maxBy (_._2))
    else None
  }
}
