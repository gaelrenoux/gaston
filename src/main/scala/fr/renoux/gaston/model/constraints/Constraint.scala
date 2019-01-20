package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.Schedule

/* TODO scores could be cached. In the Schedule, have a set of ScheduleSot, scorable separately for most. And transformations usually alter only one slot at a time. */

/**
  * Mandatory constraint on the solution. Constraints should implement [[equals()]] and [[hashCode()]] to allow
  * deduplication.
  */
trait Constraint {

  /** Should partial schedules be checked for this constraint ? */
  val isApplicableToPartialSchedule: Boolean

  /** Indicates if the constraint is respected on the given schedule. */
  def isRespected(schedule: Schedule): Boolean

  /** Obviously, the opposite of isRespected. */
  def isBroken(schedule: Schedule): Boolean = !isRespected(schedule)
}

