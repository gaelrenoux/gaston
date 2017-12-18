package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Person, Schedule, Slot, Topic}
import fr.renoux.gaston.util.RicherCollections._
import fr.renoux.gaston.util.Dispatch

import scala.util.Random

/**
  * Uses backtracking to create a solution satisfying all constraints. Does not take preferences into account.
  */
class ConstrainedScheduleFactory(val problem: Problem) {

  private val log = Logger[ConstrainedScheduleFactory]


  /** Returns a Schedule satisfying all constraints, based on given random. Returns None if such a schedule cannot be
    *  constructed. */
  def makeSchedule(implicit random: Random): Option[Schedule] = makePartialSchedule map completePartialSchedule


  /** Returns a Schedule satisfying all constraints except number constraints, based on given random. Returns None if
    * such a schedule cannot be constructed. */
  def makePartialSchedule(implicit random: Random): Option[Schedule] = {
    val slots = random.shuffle(problem.slots.toList)
    val topics = random.shuffle(problem.topics.toList)
    backtrackForConstraints(Schedule())(slots, 0, topics)
  }


  /**
    *
    * @param partialSchedule        Partial schedule we are starting from
    * @param slotsLeft              All slots we haven't handled yet
    * @param currentSlotTopicsCount Number of topics in the current slot (the head of slotsLeft)
    * @param topicsLeft             Topics we have to handle. The head is the current topic.
    * @param topicsPassed           Topics we have to handle but were already deemed unacceptable for the current solution
    * @return Some schedule that fits.
    */
  private def backtrackForConstraints(partialSchedule: Schedule)
                                     (slotsLeft: List[Slot], currentSlotTopicsCount: Int, topicsLeft: List[Topic], topicsPassed: List[Topic] = Nil): Option[Schedule] = {
    if (topicsLeft.isEmpty && topicsPassed.isEmpty) {
      log.debug("Finishing backtrackForConstraints because we have no more topics")
      Some(partialSchedule) // we have integrated all topics
    } else if (topicsLeft.isEmpty) {
      log.debug("Hit a dead end")
      None // some topics were deemed unacceptable, and we have no more topics left : no solution
    } else if (slotsLeft.isEmpty) {
      log.debug("Finishing backtrackForConstraints because we have no more slots")
      Some(partialSchedule) // some topics are left but we have no more slots
    } else {
      val currentSlot = slotsLeft.head
      val currentTopic = topicsLeft.head
      val nextTopics = topicsLeft.tail

      val record = Schedule.Record(currentSlot, currentTopic, problem.mandatoryPersonsPerTopic(currentTopic)) //new record we want to try
      val candidate = partialSchedule.copy(records = partialSchedule.records + record) // generate a new candidate with this record

      val solution =
        if (problem.isAcceptablePartial(candidate)) {
          log.trace(s"Candidate is acceptable: $candidate")
          val newSlotTopicsCount = (currentSlotTopicsCount + 1) % problem.parallelization // add one topic to current slot, go back to zero if we reach limit
          val newSlotsLeft = if (newSlotTopicsCount == 0) slotsLeft.tail else slotsLeft // if limit was reached, go to next slot

          /* If going to next slot, time to integrate all passed topics back into the process.
           * If not, there's no way it's going to work better with one more topic (schedule is even more constrained). */
          val (newTopicsLeft, newTopicsPassed) = if (newSlotTopicsCount == 0) (topicsPassed ::: nextTopics, Nil) else (nextTopics, topicsPassed)

          backtrackForConstraints(candidate)(newSlotsLeft, newSlotTopicsCount, newTopicsLeft, newTopicsPassed)

        } else {
          log.trace(s"Candidate is not acceptable: $candidate")
          None
        }

      solution orElse {
        log.trace(s"Backtracking from: $candidate")
        /* candidate was not acceptable, or went into a dead end. Let's try with next topic */
        backtrackForConstraints(partialSchedule)(slotsLeft, currentSlotTopicsCount, nextTopics, currentTopic :: topicsPassed)
      }
    }
  }


  /** Starts with a partial schedule satisfying all constraints except number constraint, and generates a random
    * schedule respecting all constraints. */
  private def completePartialSchedule(partialSchedule: Schedule)(implicit random: Random): Schedule = {

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

      val (personsAddedToReachMin, personsLeftLeft) = personsLeft.takeChunks(topicAndPersonsCount.map(_._2.needed))

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
      tps map { case (t, ps) => Schedule.Record(s, t, ps.toSet) }
    }

    val result = partialSchedule.merge(addedTriplets)
    if (!problem.isSolvedBy(result)) {
      val missedConstraints = problem.constraints.filter(!_.isRespected(result))
      throw new IllegalStateException(s"Random result violate some constraints: $missedConstraints")
    }
    result
  }

  /** List available persons on a certain slot, given an existing partial schedule */
  private def availablePersons(schedule: Schedule, slot: Slot)(implicit random: Random): Seq[Person] = {
    val personsLeftSet = problem.personsPerSlot(slot) -- schedule.personsPerSlot(slot)
    random.shuffle(personsLeftSet.toSeq)
  }


}
