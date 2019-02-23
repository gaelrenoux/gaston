package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model._

/**
  * That topic must be on any of those slots, if it is planned at all.
  */
case class TopicForcedSlot(topic: Topic, slots: Set[Slot]) extends Constraint.SlotLevel {

  override def isRespected(schedule: Schedule): Boolean =
    schedule.topicToSlot.get(topic).forall(slots.contains)

  override def isRespectedSlot(schedule: SlotSchedule): Boolean =
    slots.contains(schedule.slot) || !schedule.topics.contains(topic)

}
