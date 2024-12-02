package fr.renoux.gaston.model2

import scala.collection.mutable

/** Only used in tests to create a schedule in a clear way */
object ScheduleMaker {

  class ScheduleDef {
    val slotsDef = mutable.Set[SlotDef]()

    def add(sd: SlotDef): Unit = {
      if (slotsDef.exists(_.sid == sd.sid))
        throw new IllegalArgumentException(s"Duplicate slot ${sd.sid} in schedule")
      val addedTopics = sd.topicDefs.toSet
      val duplicateTopics = slotsDef.flatMap(_.topicDefs).filter(addedTopics)
      if (duplicateTopics.nonEmpty)
        throw new IllegalArgumentException(s"Duplicate topics ${duplicateTopics.mkString(",")} in schedule")
      slotsDef += sd
    }

    extension (sid: Int) {
      infix def slot(
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

    extension (tid: Int) {
      infix def topic(pids: PersonId*)(using countTopics: CountAll[TopicId], countPersons: CountAll[PersonId], slotDef: SlotDef): Unit = {
        slotDef.add(TopicDef(tid, pids*))
      }
    }
  }

  case class TopicDef(tid: TopicId, pids: PersonId*)(using
      countTopics: CountAll[TopicId],
      countPersons: CountAll[PersonId]
  ) {
    assert(tid.value < countTopics.value, "Topic ID to high")
    assert(pids.toSet.size == pids.size, "Person repetition in same topic")
    assert(pids.map(_.value).max < countPersons.value, "Person ID to high")
  }

  def mkSchedule(init: (CountAll[SlotId], CountAll[TopicId], CountAll[PersonId], ScheduleDef) ?=> Unit)(using CountAll[SlotId], CountAll[TopicId], CountAll[PersonId]): Schedule = {
    given scheduleDef: ScheduleDef = ScheduleDef()
    init

    val matrix = IdMatrix3.fill[SlotId, TopicId, PersonId, Boolean](false)
    scheduleDef.slotsDef.foreach { slotDef =>
      slotDef.topicDefs.foreach { topicDef =>
        topicDef.pids.foreach { pid =>
          matrix(slotDef.sid, topicDef.tid, pid) = true
        }
      }
    }
    Schedule(matrix)
  }
}

