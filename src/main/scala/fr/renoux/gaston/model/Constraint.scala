package fr.renoux.gaston.model

/**
  * Mandatory constraint on the solution. Constraints should implement [[equals()]] and [[hashCode()]] to allow
  * deduplication.
  */
trait Constraint {

  /** Can we test a partial schedule ? Typically, false for counting min numbers (because it may be reached when we add
    * more persons). */
  val isApplicableToPartialSchedule: Boolean = true

  /** Indicates if the constraint is respected on the given schedule. */
  def isRespected(schedule: Schedule): Boolean

  /** Obviously, the opposite of isRespected. */
  def isBroken(schedule: Schedule): Boolean = !isRespected(schedule)
}