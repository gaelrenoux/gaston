package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.Schedule

/**
  * Mandatory constraint of the solution.
  */
trait Constraint {
  def countBroken(schedule: Schedule): Long
  def isRespected(schedule: Schedule): Boolean
}

abstract class AbstractConstraint[Checked] extends Constraint {

  def countBroken(schedule: Schedule): Long = elementsChecked(schedule) filterNot check(schedule) size

  def isRespected(schedule: Schedule): Boolean = elementsChecked(schedule) forall check(schedule)

  def elementsChecked(schedule: Schedule): Seq[Checked]

  def check(schedule: Schedule)(checked: Checked): Boolean
}