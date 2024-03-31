package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Constraint, SlotSchedule, Topic}
import fr.renoux.gaston.util.BitSet

import scala.annotation.tailrec

/** None of those topics may be on the same slot. I.e., a slot may contain one (or none), but not several of them. */
@hardCoded
final case class TopicsNotSimultaneous(topics: BitSet[Topic]) extends Constraint.SlotLevel {

  override def isRespectedSlot(schedule: SlotSchedule): Boolean = check(schedule.topicsList)

  @tailrec
  private def check(ts: List[Topic], foundOne: Boolean = false): Boolean = ts match {
    case Nil => true
    case h :: t =>
      if (!topics.contains(h)) check(t, foundOne)
      else if (foundOne) false
      else check(t, foundOne = true)
  }

  override def equals(o: Any): Boolean = o match {
    case that: TopicsNotSimultaneous => this.topics.actualEquals(that.topics)
    case _ => false
  }

  override def hashCode(): Int = this.topics.actualHashCode

  override def toLongString: String = s"TopicsNotSimultaneous($topics)"
}

