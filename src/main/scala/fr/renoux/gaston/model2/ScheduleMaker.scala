package fr.renoux.gaston.model2

import scala.collection.mutable
import fr.renoux.gaston.model.{Schedule as OldSchedule}


/** Only used in tests to create a schedule in a clear way */
object ScheduleMaker {

  class ScheduleDef {
    val slotsDef: mutable.Set[SlotDef] = mutable.Set[SlotDef]()

    def add(sd: SlotDef): Unit = {
      if (slotsDef.exists(_.sid == sd.sid))
        throw new IllegalArgumentException(s"Duplicate slot ${sd.sid} in schedule")
      val addedTopics = sd.topicDefs.toSet
      val duplicateTopics = slotsDef.flatMap(_.topicDefs).filter(addedTopics)
      if (duplicateTopics.nonEmpty)
        throw new IllegalArgumentException(s"Duplicate topics ${duplicateTopics.mkString(",")} in schedule")
      slotsDef += sd
    }

    extension (sid: SlotId) {
      def slot(
          init: (CountAll[TopicId], CountAll[PersonId], SlotDef) ?=> Unit
      )(using scheduleDef: ScheduleDef, cs: CountAll[SlotId], ct: CountAll[TopicId], cp: CountAll[PersonId]): Unit = {
        given slotDef: SlotDef = SlotDef(sid)
        init
        scheduleDef.add(slotDef)
      }
    }
  }

  class SlotDef(val sid: SlotId)(using countSlots: CountAll[SlotId]) {
    assert(sid.value < countSlots.value, "Slot ID to high")
    val topicDefs = mutable.Set[TopicDef]()

    def add(td: TopicDef): Unit = {
      if (topicDefs.exists(_.tid == td.tid))
        throw new IllegalArgumentException(s"Duplicate topic ${td.tid} in slot $sid")
      val addedPersons = td.pids.toSet
      val duplicatePersons = topicDefs.flatMap(_.pids).filter(addedPersons)
      if (duplicatePersons.nonEmpty)
        throw new IllegalArgumentException(s"Duplicate persons ${duplicatePersons.mkString(",")} in slot $sid")
      topicDefs += td
    }

    extension (tid: TopicId) {
      def topic(
          pids: PersonId*
      )(using countTopics: CountAll[TopicId], countPersons: CountAll[PersonId], slotDef: SlotDef): Unit = {
        slotDef.add(TopicDef(tid, pids*))
      }

      def topicEmpty(using countTopics: CountAll[TopicId], countPersons: CountAll[PersonId], slotDef: SlotDef): Unit = {
        slotDef.add(TopicDef(tid))
      }
    }
  }

  case class TopicDef(tid: TopicId, pids: PersonId*)(using
      countTopics: CountAll[TopicId],
      countPersons: CountAll[PersonId]
  ) {
    assert(tid.value < countTopics.value, "Topic ID to high")
    assert(pids.toSet.size == pids.size, "Person repetition in same topic")
    assert(pids.isEmpty || pids.map(_.value).max < countPersons.value, "Person ID to high")
  }

  def mkSchedule(problem: SmallProblem)(
      init: (CountAll[SlotId], CountAll[TopicId], CountAll[PersonId], ScheduleDef) ?=> Unit
  ): Schedule = {
    import problem.given 
    given scheduleDef: ScheduleDef = ScheduleDef()
    val _ = init

    val schedule = Schedule.empty(problem)
    
    scheduleDef.slotsDef.foreach { (slotDef: SlotDef) =>
      val assignment = schedule.slotsToAssignment(slotDef.sid)
      slotDef.topicDefs.foreach { topicDef =>
        schedule.topicsToSlot(topicDef.tid) = slotDef.sid
        schedule.topicsPresent = schedule.topicsPresent + topicDef.tid
        topicDef.pids.foreach { pid =>
          schedule.personsToTopics(pid) = schedule.personsToTopics(pid) + topicDef.tid
          assignment.personsToTopic(pid) = topicDef.tid
          assignment.topicsToPersons(topicDef.tid) = assignment.topicsToPersons(topicDef.tid) + pid
        }
      }
    }

    //TODO: Add verifications that the schedule is sane
    schedule.recalculateAll()
    schedule
  }

  def fromOldSchedule(oldSchedule: OldSchedule, problem: SmallProblem, addUnassigned: Boolean = true): Schedule = {
    import problem.given

    val schedule = Schedule.empty(problem)

    if (addUnassigned) {
      problem.slotsCount.foreach { slotId =>
        schedule.topicsToSlot(slotId.value) = slotId
        schedule.topicsPresent = schedule.topicsPresent + slotId.value
      }
    }

    oldSchedule.slotSchedules.foreach { oldSlotSchedule =>
      val slotId = problem.slotsNames.unsafeContent.indexOf(oldSlotSchedule.slot.name)
      val assignment = schedule.slotsToAssignment(slotId)

      oldSlotSchedule.topics.foreach { oldTopic =>
        val topicId = problem.topicsName.unsafeContent.indexOf(oldTopic.name)
        schedule.topicsToSlot(topicId) = slotId
        schedule.topicsPresent = schedule.topicsPresent + topicId

        oldSlotSchedule.on(oldTopic).persons.foreach { oldPerson =>
          val personId = problem.personsName.unsafeContent.indexOf(oldPerson.name)
          schedule.personsToTopics(personId) = schedule.personsToTopics(personId) + topicId
          assignment.personsToTopic(personId) = topicId
          assignment.topicsToPersons(topicId) = assignment.topicsToPersons(topicId) + personId
        }
      }
    }


    val checkupResult = schedule.slowCheckup
    if (checkupResult.nonEmpty) {
      throw new IllegalArgumentException(checkupResult.mkString("Errors:\n", "\n", "\n"))
    }

    schedule.recalculateAll()
    schedule
  }

}
