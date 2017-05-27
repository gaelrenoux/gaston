package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Schedule, Slot}
import fr.renoux.gaston.util.CollectionImplicits._
import fr.renoux.gaston.util.Dispatch
import fr.renoux.gaston.util.RandomImplicits._

import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by gael on 07/05/17.
  */
class PreferredScheduleFactory(problem: Problem) {

  private val log = Logger[PreferredScheduleFactory]

  /** Starts with a partial schedule satisfying all constraints, and generates a random schedule. The new random schedule may violate some constraints. */
  def completePartialSchedule(partialSchedule: Schedule)(implicit random: Random): Schedule = {

    /* first step : solve all number constraints, which were not included in the partialSchedule */
    val additions = for (slot <- problem.slots) yield {

      val personsLeft = availablePersons(partialSchedule, slot)
      val topics = partialSchedule.topicsPerSlot(slot)

      case class PersonsCount(
                               existing: Int, //how many persons are already present
                               needed: Int, //how many persons are needed to reach the min
                               optional: Int //how many persons can we add after the min is reached (or with existing number if already higher than min)
                             )

      val topicAndPersonsCount = random.shuffle(topics.toSeq) map { t =>
        val min = problem.minNumberPerTopic.getOrElse(t, 0)
        val max = problem.maxNumberPerTopic.getOrElse(t, Int.MaxValue)
        val existing = partialSchedule.countPersonsPerTopic(t)
        val needed = math.max(0, min - existing)
        val optional = max - math.max(min, existing)
        t -> PersonsCount(existing, needed, optional)
      }

      val personsNeededCount = topicAndPersonsCount.map(_._2.needed).sum
      if (personsLeft.size < personsNeededCount) {
        throw new IllegalStateException(s"not enough persons: $personsLeft for $topicAndPersonsCount")
      }

      val personsOptionalCount = topicAndPersonsCount.map(_._2.optional).sum
      if (personsLeft.size > personsNeededCount + personsOptionalCount) {
        throw new IllegalStateException(s"too many persons: $personsLeft for $topicAndPersonsCount")
      }

      val (personsAddedToReachMin, personsLeftLeft) = personsLeft.takeWithRemainder(topicAndPersonsCount.map(_._2.needed))

      val (personsAddedToFinish, remainder) = Dispatch.equallyWithMaxes(topicAndPersonsCount.map(_._2.optional))(personsLeftLeft)

      if (remainder.nonEmpty) {
        throw new IllegalStateException(s"too many persons, somehow: $personsLeft for $topicAndPersonsCount")
      }

      val added = topicAndPersonsCount zip (personsAddedToReachMin zip personsAddedToFinish) map { case ((topic, _), (needed, optional)) =>
        topic -> (needed ++ optional)
      }

      log.debug(s"Added persons on slot $slot: $added")
      slot -> added
    }

    val addedTriplets = additions flatMap { case (s, tps) =>
      tps map { case (t, ps) => (s, t, ps.toSet) }
    }

    partialSchedule.merge(addedTriplets)
  }

  private def availablePersons(schedule: Schedule, slot: Slot)(implicit random: Random) = {
    val personsLeftSet = problem.personsPerSlot(slot) -- schedule.personsPerSlot(slot)
    random.shuffle(personsLeftSet.toList)
  }

  /** Takes a schedule and improves it so that it checks all constraints */
  @tailrec
  final def improveUntilItChecks(schedule: Schedule, limit: Long = 10000)(implicit random: Random): Schedule = {
    if (problem.isSolved(schedule)) schedule
    else if (limit < 0) throw new IllegalArgumentException
    else {
      if (limit % 1000 == 0) log.trace(s"Limit in improveUntilItChecks is $limit")
      improveUntilItChecks(randomSwap(schedule), limit - 1)
    }
  }


  def randomSwap(schedule: Schedule, limit: Int = 1000)(implicit rand: Random): Schedule = {
    if (limit < 0) throw new IllegalArgumentException(s"$schedule")

    val suitableSlots = schedule.topicsPerSlot.filter(_._2.size > 1).keySet
    if (suitableSlots.isEmpty) throw new IllegalArgumentException("Can't swap anything...")
    val slot = rand.pick(suitableSlots)
    val picks = rand.pick(schedule.triplets filter (_._1 == slot), 2)
    val t1 = picks(0)
    val t2 = picks(1)
    /* we have picked two triplets on the same random slot */

    val mandatoryPersonsT1 = problem.mandatoryPersonsPerTopic(t1._2)
    val mandatoryPersonsT2 = problem.mandatoryPersonsPerTopic(t2._2)
    val forbiddenPersonsT1 = problem.forbiddenPersonsPerTopic(t1._2)
    val forbiddenPersonsT2 = problem.forbiddenPersonsPerTopic(t2._2)

    val acceptableT1Picks = t1._3 -- mandatoryPersonsT1 -- forbiddenPersonsT2
    val acceptableT2Picks = t2._3 -- mandatoryPersonsT2 -- forbiddenPersonsT1

    if (acceptableT1Picks.isEmpty || acceptableT2Picks.isEmpty) randomSwap(schedule, limit - 1)
    else {
      val p1 = rand.pick(acceptableT1Picks)
      val p2 = rand.pick(acceptableT2Picks)

      val newT1 = t1.copy(_3 = t1._3 - p1 + p2)
      val newT2 = t2.copy(_3 = t2._3 - p2 + p1)

      schedule.copy(schedule.triplets - t1 - t2 + newT1 + newT2)
    }
  }
}
