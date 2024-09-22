package fr.renoux.gaston.model

/**
  * Mandatory constraint on the solution. Constraints should implement `equals()` and `hashCode()` to allow
  * deduplication. Constraints are only the stuff that are integral in constructing the solution, not additional
  * constraints such as people not wanting to play some game (those are handled as "soft" constraint, preferences with a
  * heavy cost).
  *
  * Constraints are only checked for sanity (ie, a failed constraint should crash the program), as the construction
  * process ensure they are respected.
  */
trait Constraint {

  /** Can we test a partial schedule ? For example, it's false for checking linked topics (because the missing topic may
    * be added later). */
  val isApplicableToPartialSchedule: Boolean = true

  /** Can we test an unfilled schedule ? For example, it's false for counting min numbers (because it may be reached
    * when we add more persons). */
  val isApplicableToUnfilledSchedule: Boolean = true

  /** Indicates if the constraint is respected on the given schedule. */
  def isRespected(schedule: Schedule): Boolean

  def toLongString: String

  def toAbstract: Product
}

object Constraint {


  /** Trait for constraints which can be evaluated slot by slot */
  trait SlotLevel extends Constraint {

    /** Indicates if the constraint is respected on the given schedule. Default implementation can be overriden. */
    override def isRespected(schedule: Schedule): Boolean = schedule.slotSchedules.forall(isRespectedSlot)

    /** Indicates if the constraint is respected on the given schedule. */
    def isRespectedSlot(schedule: SlotSchedule): Boolean
  }

}
