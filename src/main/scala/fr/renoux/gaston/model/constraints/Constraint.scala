package fr.renoux.gaston.model.constraints

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.Schedule

/**
  * Mandatory constraint of the solution.
  */
trait Constraint {
  def countBroken(schedule: Schedule): Long
  def isRespected(schedule: Schedule): Boolean
  val isApplicableToPartialSolution: Boolean
}

abstract class AbstractConstraint[Checked] extends Constraint {

  private val log = Logger[Constraint]

  override val isApplicableToPartialSolution: Boolean = true

  override def countBroken(schedule: Schedule): Long = elementsChecked(schedule) filterNot check(schedule) size

  override def isRespected(schedule: Schedule): Boolean = {
    val bool = elementsChecked(schedule) forall check(schedule)
    if (!bool) log.debug(s"Constraint $this is broken on $schedule")
    bool
  }

  def elementsChecked(schedule: Schedule): Seq[Checked]

  def check(schedule: Schedule)(checked: Checked): Boolean
}