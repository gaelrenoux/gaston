package fr.renoux.gaston.model.constraints

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.Schedule

/** Abstract class for constraints providing a basis for how to check things. Checked is an object that can be checked
  * individually, obtained by aggregating some stuff from the schedule. */
abstract class AbstractConstraint[Checked] extends Constraint {

  private val log = Logger[Constraint]

  /** Can we test a partial schedule ? Typically, false for counting min numbers (because it may be reached when we add
    * more persons). */
  override val isApplicableToPartialSchedule: Boolean = true

  /** How many times is this constraint broken on the schedule */
  override def countBroken(schedule: Schedule): Int = elementsChecked(schedule).filterNot(check).size

  /** Is this constraint respected on the schedule */
  override def isRespected(schedule: Schedule): Boolean = {
    val bool = elementsChecked(schedule).forall(check)
    if (!bool) log.debug(s"Constraint $this is broken on $schedule")
    bool
  }

  /** On a schedule, the elements to check the constraint on. */
  protected def elementsChecked(schedule: Schedule): Iterable[Checked]

  /** Check a constraint on one those elements. */
  protected def check(checked: Checked): Boolean
}
