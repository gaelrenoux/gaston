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
    backtrackAssignTopicsToSlots(Schedule(problem.parallelization))(slots, 0, topics)
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
        if (candidate.isSound && problem.isAcceptablePartial(candidate)) {
          log.trace(s"Candidate is acceptable: $candidate")
          val newSlotTopicsCount = (currentSlotTopicsCount + 1) % problem.parallelization // add one topic to current slot, go back to zero if we reach limit
          val newSlotsLeft = if (newSlotTopicsCount == 0) slotsLeft.tail else slotsLeft // if limit was reached, go to next slot
          backtrackAssignTopicsToSlots(candidate)(newSlotsLeft, newSlotTopicsCount, nextTopics ::: topicsPassed, Nil)

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

    /** Iterate over Slots, stop in case there is an incompatibility on some Slot */
    @tailrec
    def completeForSlots(slots: List[Slot], scheduleOption: Option[Schedule]): Option[Schedule] = (slots, scheduleOption) match {
      case (Nil, _) => scheduleOption
      case (_, None) => None

      case (slot :: slotsTail, Some(schedule)) =>
        /* Handle current slot */

        val personsLeftSet = problem.personsPerSlot(slot) -- schedule.personsPerSlot(slot)
        val personsLeft = random.shuffle(personsLeftSet.toSeq)

        /*
        val topicsMinMaxCurrent = random.shuffle(
          schedule.topicsPerSlot(slot) map { t =>
            val min = problem.minNumberPerTopic.getOrElse(t, 0)
            val max = problem.maxNumberPerTopic.getOrElse(t, Int.MaxValue)
            val current = schedule.countPersonsPerTopic(t)
            (t, min, max, current)
          } toSeq
        )

        Dispatch.equallyWithMaxes(topicsMinMaxCurrent.map(_._3)) */

        val topicsWithValues = schedule.topicsPerSlot(slot) map { t =>
          val min = problem.minNumberPerTopic.getOrElse(t, 0)
          val max = problem.maxNumberPerTopic.getOrElse(t, Int.MaxValue)
          val current = schedule.countPersonsPerTopic(t)

          ((t, min - current), (t, max - math.max(min, current)))
        } unzip

        val topicsNeedingMin = topicsWithValues._1 filter (_._2 > 0)
        val topicsOpenToMax = topicsWithValues._2 filter (_._2 > 0)

        val newSchedule = backtrackAssignPersonsToTopics(schedule)(topicsNeedingMin.toList, topicsOpenToMax.toList, personsLeft.toList, Nil, Nil)

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
    * @param personsSkipped Persons that needs to be assigned to topics but which have been skipped for the head of the topic list
    * @param topicsOpenToMaxDelayed Topics that have there minimum value and which have been delayed
    * @return None if no schedule is possible, Some(schedule) if possible
    */
  private def backtrackAssignPersonsToTopics(partialSchedule: Schedule)
                                            (topicsNeedingMin: List[(Topic, Int)], topicsOpenToMax: List[(Topic, Int)], personsLeft: List[Person], personsSkipped: List[Person], topicsOpenToMaxDelayed: List[(Topic, Int)]): Option[Schedule] =
    (topicsNeedingMin, topicsOpenToMax, personsLeft) match {
      case (Nil, _, Nil) if personsSkipped.isEmpty =>
        log.debug("Finishing backtrackAssignPersonsToTopics because we have no more persons and all topics have their min numbers")
        Some(partialSchedule) // no more persons left and min numbers all reached !

      case (_, _, Nil) =>
        log.trace("Hit a dead end in backtrackAssignPersonsToTopics because we have no more persons and some topics don't have their min numbers")
        None //no more persons left and min numbers are not reached

      case (Nil, Nil, _) if topicsOpenToMaxDelayed.isEmpty =>
        log.trace("Hit a dead end in backtrackAssignPersonsToTopics because we have more persons and all topics have their max numbers")
        None //more persons left and max numbers are reached

      case (Nil, Nil, _) =>
        log.trace("No more topics open to max, go again with all delayed topics")
        backtrackAssignPersonsToTopics(partialSchedule)(topicsNeedingMin, topicsOpenToMaxDelayed, personsLeft ++ personsSkipped, Nil, Nil)

      case ((topic, _) :: _, _, person :: ptail) if problem.forbiddenPersonsPerTopic(topic)(person) =>
        log.trace("Current topic has not reached min number, but current person is forbidden on it: step to the next person")
        backtrackAssignPersonsToTopics(partialSchedule)(topicsNeedingMin, topicsOpenToMax, ptail, person :: personsSkipped, topicsOpenToMaxDelayed)

      case ((topic, count) :: ttail, _, person :: ptail) =>
        log.trace("Current topic has not reached min number");
        {
          /* Add current person and try to go on, with the same topic is we need more persons, or on the next topic */
          val newSchedule = partialSchedule.addPersonToTopic(person, topic)
          if (count == 1) backtrackAssignPersonsToTopics(newSchedule)(ttail, topicsOpenToMax, ptail ++ personsSkipped, Nil, topicsOpenToMaxDelayed)
          else backtrackAssignPersonsToTopics(newSchedule)((topic, count - 1) :: ttail, topicsOpenToMax, ptail, personsSkipped, topicsOpenToMaxDelayed)
        } orElse {
          /* If going on did not work, adding current person won't work so go to the next one */
          log.trace("Backtracking in backtrackAssignPersonsToTopics")
          backtrackAssignPersonsToTopics(partialSchedule)(topicsNeedingMin, topicsOpenToMax, ptail, person :: personsSkipped, topicsOpenToMaxDelayed)
        }

      case (Nil, (topic, _) :: _, person :: ptail) if problem.forbiddenPersonsPerTopic(topic)(person) =>
        log.trace("Current topic has not reached max number, but current person is forbidden on it: step to the next person")
        backtrackAssignPersonsToTopics(partialSchedule)(Nil, topicsOpenToMax, ptail, person :: personsSkipped, topicsOpenToMaxDelayed)

      case (Nil, (topic, count) :: ttail, person :: ptail) =>
        log.trace("Current topic has not reached max number");
        {
          /* Add current person and try to go on, with the next topic (current topic goes at the end if we can have more persons) */
          val newSchedule = partialSchedule.addPersonToTopic(person, topic)
          if (count == 1) backtrackAssignPersonsToTopics(newSchedule)(Nil, ttail, ptail ++ personsSkipped, Nil, topicsOpenToMaxDelayed)
          else backtrackAssignPersonsToTopics(newSchedule)(Nil, ttail, ptail ++ personsSkipped, Nil, (topic, count - 1) :: topicsOpenToMaxDelayed)
        } orElse {
          /* If going on did not work, adding current person won't work so go to the next one */
          log.trace("Backtracking in backtrackAssignPersonsToTopics")
          backtrackAssignPersonsToTopics(partialSchedule)(topicsNeedingMin, topicsOpenToMax, ptail, person :: personsSkipped, topicsOpenToMaxDelayed)
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