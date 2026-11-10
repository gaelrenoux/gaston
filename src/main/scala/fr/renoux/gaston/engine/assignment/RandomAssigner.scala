package fr.renoux.gaston.engine.assignment

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.*
import fr.renoux.gaston.util.immutable

import scala.util.Random

/**
 * Fills an unfilled schedule with persons, ignoring preferences but respecting constraints.
 */
@immutable
final class RandomAssigner(using private val problem: Problem) {

  private val log = Logger[RandomAssigner]

  /** Starts with an unfilled schedule satisfying all constraints except number constraint (including having all
   * mandatory persons correctly assigned on their topics), and generates a random schedule respecting all constraints.
   *
   * Returns None if some slot cannot be filled, for example if too many persons are forbidden on several of the
   * available topics in one slot.
   */
  def fill(unfilledSchedule: Schedule)(using random: Random): Option[Schedule] = {
    log.debug("Starting to fill the unfilled schedule")
    val slotSchedules =
      problem.slotsList.view
        .map { slot => fillSlot(unfilledSchedule.on(slot)) }
        .takeWhile(_.nonEmpty) // it's a view: as soon as we have a None, we won't work on the following slots

    if (slotSchedules.size < problem.counts.slots) {
      log.debug("Could not fill unfilled schedule")
      None
    } else {
      log.debug("Unfilled schedule was filled")
      val scheduleMap = slotSchedules.map {
        case Some(ss) => ss.slot -> ss
        case None => throw new IllegalStateException
      }.toMap
      Some(unfilledSchedule.replaceAllSlotSchedules(scheduleMap))
    }
  }

  /** Assigns all unscheduled persons of the slot-schedule to its existing topics, respecting forbidden persons and the
   * min and max on topics. Persons already on a topic stay there. Returns None only if no such assignment exists. */
  def fillSlot(slotSchedule: SlotSchedule)(using random: Random): Option[SlotSchedule] = {
    val records = random.shuffle(slotSchedule.recordsList).toArray
    val persons = random.shuffle(slotSchedule.unscheduledPersonsList).toArray
    SlotMatcher.assign(records, persons).map(slotSchedule.addAll)
  }

}
