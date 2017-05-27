package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Person, Schedule, Slot, Topic}

import scala.collection.mutable
import scala.util.Random

/**
  * Created by gael on 07/05/17.
  */
class PreferredScheduleFactory(problem: Problem) {

  private val log = Logger[PreferredScheduleFactory]

  /** Starts with a partial schedule satisfying all constraints, and generates a random schedule. */
  def completePartialSchedule(partialSchedule: Schedule)(implicit random: Random): Schedule = {

    /* first step : solve all number constraints, which were not included in the partialSchedule */
    val additions = for (slot <- problem.slots) yield {
      var personsLeft = availablePersons(partialSchedule, slot)
      val topics = partialSchedule.topicsPerSlot(slot)

      val personsMinCountPerTopic = topics map { t => t -> problem.minNumberPerTopic.getOrElse(t, 0) } toMap

      val personsCountPerTopic = mutable.Map[Topic, Int]()
      topics.foreach(personsCountPerTopic(_) = 0)

      /* Calculate the number of persons needed to reach the min */
      val neededAdditionalPersonsCount = topics map { t =>
        val min = problem.minNumberPerTopic.getOrElse(t, 0)
        val existing = partialSchedule.countPersonsPerTopic(t)
        personsCountPerTopic(t) = existing
        t -> math.max(0, min - existing)
      } filter (_._2 > 0) toMap


      val addedPersons = mutable.Map[Topic, mutable.Set[Person]]()
      topics.foreach(addedPersons(_) = mutable.Set[Person]())
      /** Reach the min for every topic */
      for ((t, count) <- neededAdditionalPersonsCount) {
        val ps = addedPersons(t)
        ps ++= personsLeft.take(count) //TODO check size
        personsCountPerTopic(t) = personsCountPerTopic(t) + count
        personsLeft = personsLeft.drop(count)
      }

      /** Put everyone left */
      var topicSeq = random.shuffle(topics.toSeq)
      while (personsLeft.nonEmpty) {
        topicSeq = topicSeq filter { t => personsCountPerTopic(t) < problem.maxNumberPerTopic.getOrElse(t, Int.MaxValue) }
        for (t <- topicSeq) {
          if (personsLeft.nonEmpty) {
            val p = personsLeft.head
            addedPersons(t) += p
            personsCountPerTopic(t) = personsCountPerTopic(t) + 1
            personsLeft = personsLeft.tail
          }
        }
      }

      log.debug(s"Added persons on slot $slot: $addedPersons")
      slot -> addedPersons.mapValues(_.toSet).toSet
    }

    val addedTriplets = additions flatMap { case (s, tps) =>
      tps map { case (t, ps) => (s, t, ps) }
    }

    partialSchedule.merge(addedTriplets)
  }

  private def availablePersons(schedule: Schedule, slot: Slot)(implicit random: Random) = {
    val personsLeftSet = problem.personsPerSlot(slot) -- schedule.personsPerSlot(slot)
    random.shuffle(personsLeftSet.toList)
  }

}
