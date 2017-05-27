package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.Schedule

/**
  * The more preferences are satisfied the better, but it's not mandatory.
  */
trait Preference {
  def strength: Preference.Strength

  def countSatisfied(schedule: Schedule): Int
}

abstract class AbstractPreference[Checked] extends Preference {

  def countSatisfied(schedule: Schedule): Int = elementsChecked(schedule) count check(schedule)

  /** On a schedule, what are the elements to look at to now if it's working. */
  def elementsChecked(schedule: Schedule): Seq[Checked]

  /** How to check if one of those elements is OK */
  def check(schedule: Schedule)(checked: Checked): Boolean
}

object Preference {

  sealed abstract class Strength

  object Strong extends Strength

  object Weak extends Strength

}