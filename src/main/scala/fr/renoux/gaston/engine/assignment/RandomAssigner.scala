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
    val records = random.shuffle(slotSchedule.recordsList)
    backtrackFillPersons(slotSchedule)(records, personsLeft)
  }

  /**
    * First, fills in persons on topics that need to reach their minimum number of persons. Then, delegates filling up
    * the rest to assignRemainingPersons.
    * @param recordsInNeed Records where the topic needs more people to reach its min.
    * @param personsLeft Persons that can be assigned to the head of recordsInNeed.
    * @param personsSkipped Persons that have been looked at and skipped for the head of topicsInNeed.
    */
  private def backtrackFillPersons(partialSlotSchedule: SlotSchedule)
    (recordsInNeed: List[Record], personsLeft: List[Person], personsSkipped: List[Person] = Nil)
    (implicit random: Random): Option[SlotSchedule] =
    (recordsInNeed, personsLeft) match {
      case (Nil, _) =>
        // all topics in need were filled. We know need to assign the remaining persons
        val records = random.shuffle(partialSlotSchedule.recordsList)
        assignRemainingPersons(partialSlotSchedule)(personsLeft ++ personsSkipped, records)

      case (record :: otherRecords, _) if !record.requiresMorePersons =>
        // current record is ready, remove it entirely and go to the next one
        backtrackFillPersons(partialSlotSchedule)(otherRecords, personsSkipped ++ personsLeft)

      case (_, Nil) =>
        // we've reached a point where we cannot complete the head record. There's no solution from here.
        None

      case (record :: _, person :: otherPersons) if record.topic.forbidden.contains(person) =>
        // skip the current person and move to the next one
        backtrackFillPersons(partialSlotSchedule)(recordsInNeed, otherPersons, person :: personsSkipped)

      case (record :: otherRecords, person :: otherPersons) =>
        // Try to add the current person, then continue on the same record. If it fails, skip the current person instead.
        val newSchedule = partialSlotSchedule.addPersonToExistingTopic(record.topic, person)
        val newRecord = newSchedule.on(record.topic) // need to replace the old record with the new one!
        backtrackFillPersons(newSchedule)(newRecord :: otherRecords, otherPersons, personsSkipped) orElse
          backtrackFillPersons(partialSlotSchedule)(recordsInNeed, otherPersons, person :: personsSkipped)
    }

  /**
    * Fills in persons on topics that can still take more persons. Assumes that the provided slot-schedule has enough persons on each topic to reach the min.
    * @param personsLeft Persons that we still need to assign.
    * @param recordsOpen Records where we can add the head of personsLeft.
    * @param recordsSkipped Records that have been looked at and skipped for the head of personsLeft.
    */
  private def assignRemainingPersons(partialSlotSchedule: SlotSchedule)
    (personsLeft: List[Person], recordsOpen: List[Record], recordsSkipped: List[Record] = Nil): Option[SlotSchedule] =
    (personsLeft, recordsOpen) match {
      case (Nil, _) =>
        // No one left to assign, current slot-schedule is a go
        Some(partialSlotSchedule)

      case (_, Nil) =>
        // No open topics available anymore, but we still have persons to assign. There's no solution from here.
        None

      case (_, record :: otherRecords) if !record.canAddPersons =>
        // Record is full, remove it entirely from the list
        assignRemainingPersons(partialSlotSchedule)(personsLeft, otherRecords, recordsSkipped)

      case (person :: _, record :: otherRecords) if record.topic.forbidden.contains(person) =>
        // skip the current record and move to the next one
        assignRemainingPersons(partialSlotSchedule)(personsLeft, otherRecords, record :: recordsSkipped)

      case (person :: otherPersons, record :: otherRecords) =>
        // Try to add the person to the current record, then continue. If it fails, skip the current record instead.
        val newSchedule = partialSlotSchedule.addPersonToExistingTopic(record.topic, person)
        val newRecord = newSchedule.on(record.topic) // need to replace the old record with the new one!
        assignRemainingPersons(newSchedule)(otherPersons, newRecord :: (recordsSkipped ++ otherRecords)) orElse
          assignRemainingPersons(partialSlotSchedule)(personsLeft, otherRecords, record :: recordsSkipped)
    }


}
