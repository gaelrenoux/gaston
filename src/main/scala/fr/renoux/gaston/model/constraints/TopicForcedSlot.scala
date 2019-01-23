package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Schedule, Slot, Topic}

/**
  * That topic must be on any of those slots.
  */
case class TopicForcedSlot(topic: Topic, slots: Set[Slot]) extends AbstractConstraint[(Slot, Set[Topic])] {

  override protected def elementsChecked(schedule: Schedule): Iterable[(Slot, Set[Topic])] = schedule.topicsPerSlot

  override protected def check(checked: (Slot, Set[Topic])): Boolean = {
    val (s, ts) = checked
    !ts.contains(topic) || slots.contains(s)
  }

}
