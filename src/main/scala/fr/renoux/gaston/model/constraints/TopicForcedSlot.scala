package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Constraint, Schedule, Slot, Topic}

/**
  * That topic must be on any of those slots, if it is planned at all.
  */
case class TopicForcedSlot(topic: Topic, slots: Set[Slot]) extends Constraint {

  /** Is this constraint respected on the schedule */
  override def isRespected(schedule: Schedule): Boolean =
    schedule.topicToSlot.get(topic).forall(slots.contains)

}
