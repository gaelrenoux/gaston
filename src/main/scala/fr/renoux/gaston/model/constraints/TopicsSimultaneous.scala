package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Constraint, Schedule, SlotSchedule, Topic}

/** Those two topics must be on the same slot. I.e., a slot must either contain both or none. */
@hardCoded
case class TopicsSimultaneous(topics: Set[Topic]) extends Constraint.SlotLevel {

  private val topicsSize = topics.size

  /** Is this constraint respected on the schedule */
  override def isRespected(schedule: Schedule): Boolean = {
    schedule.topicsPerSlot.values.forall { ts =>
      val missing = topics.diff(ts)
      missing.isEmpty || missing.size == topicsSize
    }
  }

  override def isRespectedSlot(schedule: SlotSchedule): Boolean = {
    val missing = topics.diff(schedule.topics)
    missing.isEmpty || missing.size == topicsSize
  }
}

