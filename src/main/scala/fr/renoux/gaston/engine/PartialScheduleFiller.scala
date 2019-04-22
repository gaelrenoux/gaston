package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model._

import scala.annotation.tailrec
import scala.util.Random

/**
  * Fills a partial schedule with persons, ignoring preferences but respecting constraints.
  */
class PartialScheduleFiller(val problem: Problem) {

  private val log = Logger[PartialSchedulesGenerator]

  /** Starts with a partial schedule satisfying all constraints except number constraint, and generates a random
    * schedule respecting all constraints. Returns None if such a schedule cannot be constructed (e.g. too many people
    * forbidden on a topic).
    */
  def fill(partialSchedule: Schedule)(implicit random: Random): Option[Schedule] = {

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

        val topicsWithValues = schedule.topicsPerSlot(slot).map { t =>
          val min = problem.minNumberPerTopic.getOrElse(t, 0)
          val max = problem.maxNumberPerTopic.getOrElse(t, Int.MaxValue)
          val current = schedule.countPersonsPerTopic(t)

          ((t, min - current), (t, max - math.max(min, current)))
        }.unzip

        val topicsNeedingMin = topicsWithValues._1 filter (_._2 > 0)
        val topicsOpenToMax = topicsWithValues._2 filter (_._2 > 0)

        val newSchedule = backtrackAssignPersonsToTopics(schedule)(topicsNeedingMin.toList, topicsOpenToMax.toList, personsLeft.toList, Nil, Nil)

        completeForSlots(slotsTail, newSchedule)
    }

    log.debug("Starting to fill the partial schedule")

    /* check wether it's possible to make it work first */
    val filled = partialSchedule.topicsPerSlot.find { case (slot, topics) =>
      val min = topics.view.map(problem.minNumberPerTopic(_)).sum
      val max = topics.view.map(problem.maxNumberPerTopic(_)).sum
      val pCount = problem.personsCountPerSlot(slot)
      pCount < min || pCount > max
    } match {
      case None => completeForSlots(problem.slots.toList, Some(partialSchedule))
      case Some((slot, _)) =>
        log.trace(s"Impossible to fill slot $slot")
        None
    }

    log.debug (if (filled.isDefined) "Partial schedule was filled" else "Could not fill partial schedule")

    filled
  }


  /**
    * Assign persons to a list of topics.
    * @param partialSchedule Partial schedule from which we start
    * @param topicsNeedingMin Topics to which we need to add people to reach the min number of persons
    * @param topicsOpenToMax Topics to which we can add people up to the the max number of persons
    * @param personsLeft Persons that need to be assigned to topics
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
          val newSchedule = partialSchedule.addPersonToExistingTopic(topic, person)
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
          val newSchedule = partialSchedule.addPersonToExistingTopic(topic, person)
          if (count == 1) backtrackAssignPersonsToTopics(newSchedule)(Nil, ttail, ptail ++ personsSkipped, Nil, topicsOpenToMaxDelayed)
          else backtrackAssignPersonsToTopics(newSchedule)(Nil, ttail, ptail ++ personsSkipped, Nil, (topic, count - 1) :: topicsOpenToMaxDelayed)
        } orElse {
          /* If going on did not work, adding current person won't work so go to the next one */
          log.trace("Backtracking in backtrackAssignPersonsToTopics")
          backtrackAssignPersonsToTopics(partialSchedule)(topicsNeedingMin, topicsOpenToMax, ptail, person :: personsSkipped, topicsOpenToMaxDelayed)
        }
    }

}

