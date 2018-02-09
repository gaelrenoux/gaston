package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Schedule, Slot, Topic}

/**
  * A person must be assigned to an instance of this topic.
  */
case class TopicForcedSlot(topic: Topic, slot: Slot) extends AbstractConstraint[(Schedule, Slot, Set[Topic])] {

  override protected def elementsChecked(schedule: Schedule): Iterable[(Schedule, Slot, Set[Topic])] =
    schedule.topicsPerSlot map {
      case (s, ts) => (schedule, s, ts)
    }

  override protected def check(checked: (Schedule, Slot, Set[Topic])): Boolean = {
    /* third case is when there is still some place left */
    slot != checked._2 || checked._3(topic) || checked._3.size < checked._1.parallelization
  }

}
