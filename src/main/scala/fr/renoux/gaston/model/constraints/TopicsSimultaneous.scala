package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Constraint, SlotSchedule, Topic}

/** All those topics must be on the same slot. I.e., a slot must either contain both or none. */
@hardCoded
final case class TopicsSimultaneous(topics: Set[Topic]) extends Constraint.SlotLevel {
  //TODO maybe topics should be a BitSet?

  private val topicsSize = topics.size
  assert(topicsSize > 1)

  override val isApplicableToPartialSchedule: Boolean = false

  override def isRespectedSlot(schedule: SlotSchedule): Boolean = {
    val missing = topics.diff(schedule.topicsSet)
    missing.isEmpty || missing.size == topicsSize
  }

  override lazy val toLongString: String = s"TopicsSimultaneous(${topics.map(_.toShortString)})"

  override def toAbstract: Product = ("TopicsSimultaneous", topics.map(_.id).toSeq.sorted)
}

