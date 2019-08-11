package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Constraint, Schedule, SlotSchedule, Topic}

import scala.annotation.tailrec

/** None of those topics may be on the same slot. I.e., a slot may contain one (or none), but not several of them. */
@hardCoded
case class TopicsNotSimultaneous(topics: Set[Topic]) extends Constraint.SlotLevel {

  /** Is this constraint respected on the schedule */
  override def isRespected(schedule: Schedule): Boolean =
    schedule.topicsPerSlot.values.forall(ts => check(ts.toList))
  
  override def isRespectedSlot(schedule: SlotSchedule): Boolean = check(schedule.topicsList)

  @tailrec
  private def check(ts: List[Topic], foundOne: Boolean = false): Boolean = ts match {
    case Nil => true
    case h::t =>
      if (!topics.contains(h)) check(t, foundOne)
      else if (foundOne) false
      else check(t, foundOne = true)
  }
}

