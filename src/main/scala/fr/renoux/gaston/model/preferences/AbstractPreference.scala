package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.Schedule

/**
  * Abstract class for preferences providing a basis for how to check things. Checked is an instance that can be checked
  * individually, obtained by aggregating some stuff from the schedule. */
abstract class AbstractPreference[Checked] extends Preference {

  def countSatisfied(schedule: Schedule): Int = elementsChecked(schedule) count check

  /** On a schedule, what are the elements to look at to now if it's working. */
  def elementsChecked(schedule: Schedule): Iterable[Checked]

  /** How to check if one of those elements is OK */
  def check(checked: Checked): Boolean
}
