package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Constraint, Schedule, Topic}

/** Those two topics must be on the same slot. I.e., a slot must either contain both or none. */
case class TopicsSimultaneous(topics: Set[Topic]) extends Constraint {

  private val topicsSize = topics.size

  /** Is this constraint respected on the schedule */
  override def isRespected(schedule: Schedule): Boolean = {
    schedule.topicsPerSlot.values.forall { ts =>
      val left = topics.diff(ts)
      left.isEmpty || left.size == topicsSize
    }
  }
}

