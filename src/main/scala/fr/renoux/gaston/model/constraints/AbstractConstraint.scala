package fr.renoux.gaston.model.constraints

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.Schedule

/** Abstract class for constraints providing a basis for how to check things. Checked is an object that can be checked
  * individually, obtained by aggregating some stuff from the schedule. */
abstract class AbstractConstraint[Checked] extends Constraint {

  private val log = Logger[Constraint]

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
