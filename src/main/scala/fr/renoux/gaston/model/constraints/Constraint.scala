package fr.renoux.gaston.model.constraints

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.Schedule

/**
  * Mandatory constraint on the solution.
  */
trait Constraint {
  def countBroken(schedule: Schedule): Int
  def isRespected(schedule: Schedule): Boolean
  val isApplicableToPartialSolution: Boolean
}

abstract class AbstractConstraint[Checked] extends Constraint {

  private val log = Logger[Constraint]

  /** Can we test a partial schedule ? Typically false for counting min numbers (because it may be reached when we add more persons). */
  override val isApplicableToPartialSolution: Boolean = true

  /** How many times is this constraint broken on the schedule */
  override def countBroken(schedule: Schedule): Int = elementsChecked(schedule) filterNot check(schedule) size

  /** Is this constraint respected on the schedule */
  override def isRespected(schedule: Schedule): Boolean = {
    val bool = elementsChecked(schedule) forall check(schedule)
    if (!bool) log.debug(s"Constraint $this is broken on $schedule")
    bool
  }

  /** On a schedule, what are the elements to look at to now if it's working. */
  def elementsChecked(schedule: Schedule): Seq[Checked]

  /** How to check if one of those elements is OK */
  def check(schedule: Schedule)(checked: Checked): Boolean
}