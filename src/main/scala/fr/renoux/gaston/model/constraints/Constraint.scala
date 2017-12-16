package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.Schedule

/**
  * Mandatory constraint on the solution.
  */
trait Constraint {

  /** Must be defined by the constraint class: should partial solutions check this constraint ? */
  val isApplicableToPartialSolution: Boolean

  /** Counts how many times this constraint is borken on the given schedule */
  def countBroken(schedule: Schedule): Int

  /** Indicates if the constraint is respected on the given schedule. Equivalent to having #countBroken return 0, but
    * faster (since in case of a failure we stop at the first occurence). */
  def isRespected(schedule: Schedule): Boolean
}

