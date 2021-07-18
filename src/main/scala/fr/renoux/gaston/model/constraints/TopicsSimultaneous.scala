package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Constraint, SlotSchedule, Topic}
import fr.renoux.gaston.util.ArraySet

/** All those topics must be on the same slot. I.e., a slot must either contain both or none. */
@hardCoded
final case class TopicsSimultaneous(topics: ArraySet[Topic]) extends Constraint.SlotLevel {

  private val topicsSize = topics.size
  assert(topicsSize > 1)

  override def isRespectedSlot(schedule: SlotSchedule): Boolean = {
    val missing = topics -- schedule.topicsSet
    missing.isEmpty || missing.size == topicsSize
  }

  override def equals(o: Any): Boolean = o match {
    case that: TopicsSimultaneous => topics === that.topics
    case _ => false
  }

  override def hashCode(): Int = topics.actualHashCode
}

