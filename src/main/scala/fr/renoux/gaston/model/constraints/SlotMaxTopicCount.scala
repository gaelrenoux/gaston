package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model._

/** Max number of topics on this specfic slot. */
case class SlotMaxTopicCount(slot: Slot, count: Int) extends Constraint.SlotLevel {

  override def isRespected(schedule: Schedule): Boolean =
    schedule.topicsPerSlot.mapValues(_.size).getOrElse(slot, 0) <= count

  override def isRespectedSlot(schedule: SlotSchedule): Boolean =
    schedule.topics.size <= count

}
