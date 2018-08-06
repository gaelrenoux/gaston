package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Schedule, Slot, Topic}

/** Those two topics must be on the same slot. I.e., a slot must either contain both or none. */
class LinkedTopics(t1: Topic, t2: Topic) extends AbstractConstraint[Set[Topic]] {

  override protected def elementsChecked(schedule: Schedule): Iterable[Set[Topic]] = schedule.topicsPerSlot.values

  override protected def check(checked: Set[Topic]): Boolean = checked(t1) == checked(t2)
}

