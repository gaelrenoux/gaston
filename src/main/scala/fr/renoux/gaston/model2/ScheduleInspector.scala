package fr.renoux.gaston.model2

import fr.renoux.gaston.util.*

import scala.collection.MapView

final class ScheduleInspector(val schedule: Schedule) {
  val problem = schedule.problem
  import problem.given

  val slotsToTopicsToPersons: Map[SlotId, Map[TopicId, Set[PersonId]]] = schedule.slotsToAssignment.toMap.mapValuesStrict { ass =>
    ass.topicsToPersons.toMap.mapValuesStrict(_.toSet)
  }

  val slotsToTopics: Map[SlotId, Set[TopicId]] = schedule.slotsToTopics.toMap.mapValuesStrict(_.toSet)

  def checkAll() = {
    checkPersonsPresent()
    checkTopicConsistency()
  }

  def checkTopicConsistency() = {
    problem.slotsCount.foreach { slotId =>
      val topicsFromSchedule = schedule.topicsToSlot.keysFilter { (tid, sid) => slotId == sid }.toSet
      val topicsFromAssignment = schedule.slotsToAssignment(slotId).personsToTopic.valuesSeq.toSet
      // it's not on the assignment is nobody has been assigned to it yet. However, if it's assigned, it should be in the schedule.

      val missingOnSchedule = (topicsFromAssignment -- topicsFromSchedule).map(topicLabel)
      assert(missingOnSchedule.isEmpty, s"Missing on the schedule! ${missingOnSchedule.toSeq.sorted} on ${slotLabel(slotId)}")
    }
  }

  def checkPersonsPresent() = {
    slotsToTopicsToPersons.foreach { case (slotId, assignment) =>
      val personsPresent = problem.slotsToPersonsPresent(slotId).toSet
      val personsAssigned = assignment.values.flatten.toSet

      val missingPersons = (personsPresent -- personsAssigned).map(personLabel)
      val addedPersons = (personsAssigned -- personsPresent).map(personLabel)
      assert(missingPersons.isEmpty, s"Someone's missing! ${missingPersons.toSeq.sorted} on ${slotLabel(slotId)}")
      assert(addedPersons.isEmpty, s"Someone shouldn't be there! ${addedPersons.toSeq.sorted} on ${slotLabel(slotId)}")
    }
  }

  def whereIsPerson(pid: PersonId): Map[String, Option[(String, Set[String])]] = {
    slotsToTopicsToPersons.map { case (slotId, topicToPersons) =>
      slotLabel(slotId) -> topicToPersons.find(_._2.contains(pid)).map {
        case (topicId, group) => topicLabel(topicId) -> group.map(personLabel)
      }
    }
  }

  def personLabel(pid: PersonId) = s"$pid/${problem.personsToName(pid)}"
  def topicLabel(tid: TopicId) = s"$tid/${problem.topicsToName(tid)}"
  def slotLabel(sid: SlotId) = s"$sid/${problem.slotsToName(sid)}"

}
