package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model._

import scala.annotation.tailrec
import scala.util.Random

/**
  * Fills a partial schedule with persons, ignoring preferences but respecting constraints.
  */
class PartialScheduleFiller(implicit private val problem: Problem) {

  private val log = Logger[PartialScheduleFiller]

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

        val personsLeftSet = slot.personsPresent -- schedule.personsPerSlot.getOrElse(slot, Set.empty)
        val personsLeft = random.shuffle(personsLeftSet.toSeq)

        val topics = schedule.topicsPerSlot.getOrElse(slot, Set.empty)
        val (topicsWithNeeded, topicsWithOptional) = topics.map { t =>
          val count = schedule.countPersonsPerTopic(t)
          val needed = t.min - count
          val optional = t.max - math.max(t.min, count)
          (t -> needed, t -> optional)
        }.unzip
        val topicsNeedingMin = topicsWithNeeded.filter(c => c._2 > 0)
        val topicsOpenToMax = topicsWithOptional.filter(_._2 > 0)

        val newSchedule = backtrackAssignPersonsToTopics(schedule)(topicsNeedingMin.toList, topicsOpenToMax.toList, personsLeft.toList, Nil, Nil)

        completeForSlots(slotsTail, newSchedule)
    }

    log.debug("Starting to fill the partial schedule")

    /* check wether it's possible to make it work first */
    val filled = partialSchedule.topicsPerSlot.find { case (slot, topics) =>
      val min = topics.view.map(_.min).sum
      val max = topics.view.map(_.max).sum
      val pCount = slot.personsPresentCount
      pCount < min || pCount > max
    } match {
      case None => completeForSlots(problem.slots.toList, Some(partialSchedule))
      case Some((slot, _)) =>
        log.trace(s"Impossible to fill slot $slot")
        None
    }

    log.debug(if (filled.isDefined) "Partial schedule was filled" else "Could not fill partial schedule")

    filled
  }


  /**
    * Assign persons to a list of topics.
    * @param partialSchedule Partial schedule from which we start
    * @param topicsInNeed Topics to which we need to add people to reach the min number of persons
    * @param topicsOpen Topics to which we can add people up to the the max number of persons
    * @param personsLeft Persons that need to be assigned to topics
    * @param personsSkipped Persons that needs to be assigned to topics but which have been skipped for the head of the topic list
    * @param topicsOpenDelayed Topics that have there minimum value and which have been delayed
    * @return None if no schedule is possible, Some(schedule) if possible
    */
  // scalastyle:off cyclomatic.complexity method.length
  private def backtrackAssignPersonsToTopics(
      partialSchedule: Schedule
  )(
      topicsInNeed: List[(Topic, Int)],
      topicsOpen: List[(Topic, Int)],
      personsLeft: List[Person],
      personsSkipped: List[Person],
      topicsOpenDelayed: List[(Topic, Int)]
  ): Option[Schedule] =
    (topicsInNeed, topicsOpen, personsLeft) match {
      case (Nil, _, Nil) if personsSkipped.isEmpty =>
        log.trace("Finishing backtrackAssignPersonsToTopics because we have no more persons and all topics have their min numbers")
        Some(partialSchedule) // no more persons left and min numbers all reached !

      case (_, _, Nil) =>
        log.trace("Hit a dead end in backtrackAssignPersonsToTopics because we have no more persons and some topics don't have their min numbers")
        None // no more persons left and min numbers are not reached

      case (Nil, Nil, _) if topicsOpenDelayed.isEmpty =>
        log.trace("Hit a dead end in backtrackAssignPersonsToTopics because we have more persons and all topics have their max numbers")
        None // more persons left and max numbers are reached

      case (Nil, Nil, _) =>
        log.trace("No more topics open to max, go again with all delayed topics")
        backtrackAssignPersonsToTopics(partialSchedule)(topicsInNeed, topicsOpenDelayed, personsLeft ++ personsSkipped, Nil, Nil)

      case ((topic, _) :: _, _, person :: ptail) if topic.forbidden(person) =>
        log.trace("Current topic has not reached min number, but current person is forbidden on it: step to the next person")
        backtrackAssignPersonsToTopics(partialSchedule)(topicsInNeed, topicsOpen, ptail, person :: personsSkipped, topicsOpenDelayed)

      case ((topic, count) :: ttail, _, person :: ptail) =>
        log.trace("Current topic has not reached min number");
        {
          /* Add current person and try to go on, with the same topic is we need more persons, or on the next topic */
          val newSchedule = partialSchedule.addPersonToExistingTopic(topic, person)
          if (count == 1) backtrackAssignPersonsToTopics(newSchedule)(ttail, topicsOpen, ptail ++ personsSkipped, Nil, topicsOpenDelayed) // min is reached !
          else backtrackAssignPersonsToTopics(newSchedule)((topic, count - 1) :: ttail, topicsOpen, ptail, personsSkipped, topicsOpenDelayed)
        } orElse {
          /* If going on did not work, adding current person won't work so go to the next one */
          log.trace("Backtracking in backtrackAssignPersonsToTopics")
          backtrackAssignPersonsToTopics(partialSchedule)(topicsInNeed, topicsOpen, ptail, person :: personsSkipped, topicsOpenDelayed)
        }

      case (Nil, (topic, _) :: _, person :: ptail) if topic.forbidden(person) =>
        log.trace("Current topic has not reached max number, but current person is forbidden on it: step to the next person")
        backtrackAssignPersonsToTopics(partialSchedule)(Nil, topicsOpen, ptail, person :: personsSkipped, topicsOpenDelayed)

      case (Nil, (topic, count) :: ttail, person :: ptail) =>
        log.trace("Current topic has not reached max number");
        {
          /* Add current person and try to go on, with the next topic (current topic goes at the end if we can have more persons) */
          val newSchedule = partialSchedule.addPersonToExistingTopic(topic, person)
          if (count == 1) backtrackAssignPersonsToTopics(newSchedule)(Nil, ttail, ptail ++ personsSkipped, Nil, topicsOpenDelayed)
          else backtrackAssignPersonsToTopics(newSchedule)(Nil, ttail, ptail ++ personsSkipped, Nil, (topic, count - 1) :: topicsOpenDelayed)
        } orElse {
          /* If going on did not work, adding current person won't work so go to the next one */
          log.trace("Backtracking in backtrackAssignPersonsToTopics")
          backtrackAssignPersonsToTopics(partialSchedule)(topicsInNeed, topicsOpen, ptail, person :: personsSkipped, topicsOpenDelayed)
        }
    }

  // scalastyle:on cyclomatic.complexity method.length

}
