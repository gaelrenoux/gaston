package fr.renoux.gaston.engine.assignment

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model._
import fr.renoux.gaston.util.immutable

import scala.util.Random

/**
  * Fills a partial schedule with persons, ignoring preferences but respecting constraints.
  */
@immutable
final class RandomAssigner(implicit private val problem: Problem) {

  private val log = Logger[RandomAssigner]

  /** Starts with a partial schedule satisfying all constraints except number constraint (including having all mandatory
    * persons correctly assigned on their topics), and generates a random schedule respecting all constraints, using
    * backtracking as necessary.
    *
    * Input schedule must have a workable one, where the totals of min and max on topics are compatible with the number
    * of persons present on their slot). In rare cases, it can be impossible to build a working schedule, for example
    * if too many persons are forbidden on several of the available topics in one slot.
    */
  def fill(partialSchedule: Schedule)(implicit random: Random): Option[Schedule] = {
    log.debug("Starting to fill the partial schedule")
    val slotSchedules =
      problem.slotsList.view
        .map { slot => fillSlot(partialSchedule.on(slot)) }
        .takeWhile(_.nonEmpty) // it's a view: as soon as we have a None, we won't work on the following slots

    if (slotSchedules.size < problem.counts.slots) {
      log.debug("Could not fill partial schedule")
      None
    } else {
      log.debug("Partial schedule was filled")
      val scheduleMap = slotSchedules.map { case Some(ss) => ss.slot -> ss }.toMap
      Some(partialSchedule.replaceAllSlotSchedules(scheduleMap))
    }
  }

  @inline
  private def fillSlot(slotSchedule: SlotSchedule)(implicit random: Random): Option[SlotSchedule] = {
    val personsLeft = random.shuffle(slotSchedule.unscheduledPersonsList)
    val topics = slotSchedule.topics

    val (topicsWithNeeded, topicsWithOptional) = topics.map { t =>
      val count = slotSchedule.countPersonsByTopic(t)
      val needed = t.min - count
      val optional = t.max - math.max(t.min, count)
      (t -> needed, t -> optional)
    }.unzip

    val topicsNeedingMin = topicsWithNeeded.filter(c => c._2 > 0)
    val topicsOpenToMax = topicsWithOptional.filter(_._2 > 0)

    backtrackAssignPersonsToTopics(slotSchedule)(topicsNeedingMin.toList, topicsOpenToMax.toList, personsLeft, Nil, Nil)
  }


  /**
    * Assign persons to a list of topics.
    * @param partialSlotSchedule Partial slot schedule from which we start
    * @param topicsInNeed Topics to which we need to add people to reach the min number of persons
    * @param topicsOpen Topics to which we can add people up to the the max number of persons
    * @param personsLeft Persons that need to be assigned to topics
    * @param personsSkipped Persons that needs to be assigned to topics but which have been skipped for the head of the topic list
    * @param topicsOpenDelayed Topics that have there minimum value and which have been delayed
    * @return None if no schedule is possible, Some(schedule) if possible
    */
  // scalastyle:off cyclomatic.complexity method.length
  private def backtrackAssignPersonsToTopics(
      partialSlotSchedule: SlotSchedule
  )(
      topicsInNeed: List[(Topic, Int)],
      topicsOpen: List[(Topic, Int)],
      personsLeft: List[Person],
      personsSkipped: List[Person],
      topicsOpenDelayed: List[(Topic, Int)]
  ): Option[SlotSchedule] =
    (topicsInNeed, topicsOpen, personsLeft) match {
      case (Nil, _, Nil) if personsSkipped.isEmpty =>
        log.trace("Finishing backtrackAssignPersonsToTopics because we have no more persons and all topics have their min numbers")
        Some(partialSlotSchedule) // no more persons left and min numbers all reached !

      case (_, _, Nil) =>
        log.trace("Hit a dead end in backtrackAssignPersonsToTopics because we have no more persons and some topics don't have their min numbers")
        None // no more persons left and min numbers are not reached

      case (Nil, Nil, _) if topicsOpenDelayed.isEmpty =>
        log.trace("Hit a dead end in backtrackAssignPersonsToTopics because we have more persons and all topics have their max numbers")
        None // more persons left and max numbers are reached

      case (Nil, Nil, _) =>
        log.trace("No more topics open to max, go again with all delayed topics")
        backtrackAssignPersonsToTopics(partialSlotSchedule)(topicsInNeed, topicsOpenDelayed, personsLeft ++ personsSkipped, Nil, Nil)

      case ((topic, _) :: _, _, person :: ptail) if topic.forbidden(person) =>
        log.trace("Current topic has not reached min number, but current person is forbidden on it: step to the next person")
        backtrackAssignPersonsToTopics(partialSlotSchedule)(topicsInNeed, topicsOpen, ptail, person :: personsSkipped, topicsOpenDelayed)

      case ((topic, count) :: ttail, _, person :: ptail) =>
        log.trace("Current topic has not reached min number");
        {
          /* Add current person and try to go on, with the same topic is we need more persons, or on the next topic */
          val newSchedule = partialSlotSchedule.addPersonToExistingTopic(topic, person)
          if (count == 1) backtrackAssignPersonsToTopics(newSchedule)(ttail, topicsOpen, ptail ++ personsSkipped, Nil, topicsOpenDelayed) // min reached !
          else backtrackAssignPersonsToTopics(newSchedule)((topic, count - 1) :: ttail, topicsOpen, ptail, personsSkipped, topicsOpenDelayed)
        } orElse {
          /* If going on did not work, adding current person won't work so go to the next one */
          log.trace("Backtracking in backtrackAssignPersonsToTopics")
          backtrackAssignPersonsToTopics(partialSlotSchedule)(topicsInNeed, topicsOpen, ptail, person :: personsSkipped, topicsOpenDelayed)
        }

      case (Nil, (topic, _) :: _, person :: ptail) if topic.forbidden(person) =>
        log.trace("Current topic has not reached max number, but current person is forbidden on it: step to the next person")
        backtrackAssignPersonsToTopics(partialSlotSchedule)(Nil, topicsOpen, ptail, person :: personsSkipped, topicsOpenDelayed)

      case (Nil, (topic, count) :: ttail, person :: ptail) =>
        log.trace("Current topic has not reached max number");
        {
          /* Add current person and try to go on, with the next topic (current topic goes at the end if we can have more persons) */
          val newSchedule = partialSlotSchedule.addPersonToExistingTopic(topic, person)
          if (count == 1) backtrackAssignPersonsToTopics(newSchedule)(Nil, ttail, ptail ++ personsSkipped, Nil, topicsOpenDelayed)
          else backtrackAssignPersonsToTopics(newSchedule)(Nil, ttail, ptail ++ personsSkipped, Nil, (topic, count - 1) :: topicsOpenDelayed)
        } orElse {
          /* If going on did not work, adding current person won't work so go to the next one */
          log.trace("Backtracking in backtrackAssignPersonsToTopics")
          backtrackAssignPersonsToTopics(partialSlotSchedule)(topicsInNeed, topicsOpen, ptail, person :: personsSkipped, topicsOpenDelayed)
        }
    }

  // scalastyle:on cyclomatic.complexity method.length

}
