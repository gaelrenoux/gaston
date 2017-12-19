package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Person, Schedule, Slot, Topic}

import scala.annotation.tailrec
import scala.util.Random

/**
  * Uses backtracking to create a solution satisfying all constraints. Does not take preferences into account.
  */
class ConstrainedScheduleFactory(val problem: Problem) {

  private val log = Logger[ConstrainedScheduleFactory]


  /** Returns a Schedule satisfying all constraints, based on given random. Returns None if such a schedule cannot be
    *  constructed. */
  def makeSchedule(implicit random: Random): Option[Schedule] = makePartialSchedule flatMap completePartialSchedule


  /** Returns a Schedule satisfying all constraints except number constraints, based on given random. Returns None if
    * such a schedule cannot be constructed. */
  private def makePartialSchedule(implicit random: Random): Option[Schedule] = {
    val slots = random.shuffle(problem.slots.toList)
    val topics = random.shuffle(problem.topics.toList)
    backtrackAssignTopicsToSlots(Schedule())(slots, 0, topics)
  }


  /**
    * Uses backtracking to construct a partial schedule with all topics assigned to slots, and mandatory people assigned to their topics.
    *
    * @param partialSchedule        Partial schedule we are starting from
    * @param slotsLeft              All slots we haven't handled yet
    * @param currentSlotTopicsCount Number of topics in the current slot (the head of slotsLeft)
    * @param topicsLeft             Topics we have to handle. The head is the current topic.
    * @param topicsPassed           Topics we have to handle but were already deemed unacceptable for the current solution
    * @return Some schedule that fits.
    */
  private def backtrackAssignTopicsToSlots(partialSchedule: Schedule)
                                          (slotsLeft: List[Slot], currentSlotTopicsCount: Int, topicsLeft: List[Topic], topicsPassed: List[Topic] = Nil): Option[Schedule] = {
    if (topicsLeft.isEmpty && topicsPassed.isEmpty) {
      log.debug("Finishing backtrackAssignTopicsToSlots because we have no more topics")
      Some(partialSchedule) // we have integrated all topics
    } else if (topicsLeft.isEmpty) {
      log.trace("Hit a dead end")
      None // some topics were deemed unacceptable, and we have no more topics left : no solution
    } else if (slotsLeft.isEmpty) {
      log.debug("Finishing backtrackAssignTopicsToSlots because we have no more slots")
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

          backtrackAssignTopicsToSlots(candidate)(newSlotsLeft, newSlotTopicsCount, newTopicsLeft, newTopicsPassed)

        } else {
          log.trace(s"Candidate is not acceptable: $candidate")
          None
        }

      solution orElse {
        log.trace(s"Backtracking from: $candidate")
        /* candidate was not acceptable, or went into a dead end. Let's try with next topic */
        backtrackAssignTopicsToSlots(partialSchedule)(slotsLeft, currentSlotTopicsCount, nextTopics, currentTopic :: topicsPassed)
      }
    }
  }


  /** Starts with a partial schedule satisfying all constraints except number constraint, and generates a random
    * schedule respecting all constraints. */
  private def completePartialSchedule(partialSchedule: Schedule)(implicit random: Random): Option[Schedule] = {

    @tailrec
    def completeForSlots(slots: List[Slot], scheduleOption: Option[Schedule]): Option[Schedule] = (slots, scheduleOption) match {
      case (Nil, _) => scheduleOption
      case (_, None) => None

      case (slot :: slotsTail, Some(schedule)) =>

        val personsLeftSet = problem.personsPerSlot(slot) -- schedule.personsPerSlot(slot)
        val personsLeft = random.shuffle(personsLeftSet.toSeq)

        val topicsWithValues = schedule.topicsPerSlot(slot) map { t =>
          val min = problem.minNumberPerTopic.getOrElse(t, 0)
          val max = problem.maxNumberPerTopic.getOrElse(t, Int.MaxValue)
          val current = schedule.countPersonsPerTopic(t)

          ((t, min - current), (t, max - math.max(min, current)))
        } unzip

        val topicsNeedingMin = topicsWithValues._1 filter (_._2 > 0)
        val topicsOpenToMax = topicsWithValues._2 filter (_._2 > 0)

        val newSchedule = backtrackAssignPersonsToTopics(schedule)(topicsNeedingMin.toList, topicsOpenToMax.toList, personsLeft.toList)

        completeForSlots(slotsTail, newSchedule)
    }

    completeForSlots(problem.slots.toList, Some(partialSchedule))
  }


  /**
    * Assign persons to a list of topics.
    *
    * @param partialSchedule   Partial schedule from which we start
    * @param topicsNeedingMin  Topics to which we need to add people to reach the min number of persons
    * @param topicsOpenToMax   Topics to which we can add people up to the the max number of persons
    * @param personsLeft       Persons that need to be assigned to topics
    * @param personsLeftPassed Persons that needs to be assigned to topics but which have been passed for the head of the topic list
    * @return None if no schedule is possible, Some(schedule) if possible
    */
  private def backtrackAssignPersonsToTopics(partialSchedule: Schedule)
                                            (topicsNeedingMin: List[(Topic, Int)], topicsOpenToMax: List[(Topic, Int)], personsLeft: List[Person], personsLeftPassed: List[Person] = Nil): Option[Schedule] =
    (topicsNeedingMin, topicsOpenToMax, personsLeft) match {
      case (Nil, _, Nil) =>
        log.debug("Finishing backtrackAssignPersonsToTopics because we have no more persons and all topics have their min numbers")
        Some(partialSchedule) // no more persons left and min numbers all reached !

      case (_, _, Nil) =>
        log.trace("Hit a dead end in backtrackAssignPersonsToTopics because we have no more persons and some topics don't have their min numbers")
        None //no more persons left and min numbers are not reached

      case (Nil, Nil, _) =>
        log.trace("Hit a dead end in backtrackAssignPersonsToTopics because we have more persons and all topics have their max numbers")
        None //more persons left and max numbers are reached

      case ((topic, _) :: _, _, person :: ptail) if problem.forbiddenPersonsPerTopic(topic)(person) =>
        backtrackAssignPersonsToTopics(partialSchedule)(topicsNeedingMin, topicsOpenToMax, ptail, person :: personsLeftPassed)

      case ((topic, count) :: ttail, _, person :: ptail) =>
        val newSchedule = partialSchedule.addPersonToTopic(person, topic);
        {
          if (count == 1) backtrackAssignPersonsToTopics(newSchedule)(ttail, topicsOpenToMax, ptail ++ personsLeftPassed, Nil)
          else backtrackAssignPersonsToTopics(newSchedule)((topic, count - 1) :: ttail, topicsOpenToMax, ptail, personsLeftPassed)
        } orElse {
          log.trace("Backtracking in backtrackAssignPersonsToTopics")
          backtrackAssignPersonsToTopics(partialSchedule)(topicsNeedingMin, topicsOpenToMax, ptail, person :: personsLeftPassed)
        }

      case (Nil, (topic, _) :: _, person :: ptail) if problem.forbiddenPersonsPerTopic(topic)(person) =>
        backtrackAssignPersonsToTopics(partialSchedule)(Nil, topicsOpenToMax, ptail, person :: personsLeftPassed)

      case (Nil, (topic, count) :: ttail, person :: ptail) =>
        val newSchedule = partialSchedule.addPersonToTopic(person, topic);
        {
          if (count == 1) backtrackAssignPersonsToTopics(newSchedule)(Nil, ttail, ptail ++ personsLeftPassed, Nil)
          else backtrackAssignPersonsToTopics(newSchedule)(Nil, (topic, count - 1) :: ttail, ptail, personsLeftPassed)
        } orElse {
          log.trace("Backtracking in backtrackAssignPersonsToTopics")
          backtrackAssignPersonsToTopics(partialSchedule)(topicsNeedingMin, topicsOpenToMax, ptail, person :: personsLeftPassed)
        }
    }

}

object ConstrainedScheduleFactory {

  /** Counting people on a certain topic */
  private case class PersonsCount(
                                   existing: Int, //how many persons are already present
                                   needed: Int, //how many persons are needed to reach the min
                                   optional: Int //how many persons can we add after the min is reached (or with existing number if already higher than min)
                                 )


}