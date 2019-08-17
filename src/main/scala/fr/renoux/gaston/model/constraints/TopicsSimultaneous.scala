package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Constraint, SlotSchedule, Topic}

/** All those topics must be on the same slot. I.e., a slot must either contain both or none. */
@hardCoded
case class TopicsSimultaneous(topics: Set[Topic]) extends Constraint.SlotLevel {

  private val topicsSize = topics.size

  override def isRespectedSlot(schedule: SlotSchedule): Boolean = {
    val missing = topics.diff(schedule.topics)
    missing.isEmpty || missing.size == topicsSize
  }
}

