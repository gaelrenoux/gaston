package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Schedule, Topic}

/** Those two topics must be on the same slot. I.e., a slot must either contain both or none. */
case class TopicsSimultaneous(topics: Set[Topic]) extends AbstractConstraint[Set[Topic]] {

  override protected def elementsChecked(schedule: Schedule): Iterable[Set[Topic]] = schedule.topicsPerSlot.values

  override protected def check(topicsOnSameSlot: Set[Topic]): Boolean =
    topics.forall(topicsOnSameSlot) || !topics.exists(topicsOnSameSlot)
}

