package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.Schedule

/**
  * Created by gael on 07/05/17.
  */
trait Preference {
  def strength: Preference.Strength

  def countSatisfied(schedule: Schedule): Long
}

abstract class AbstractPreference[Checked] extends Preference {

  def countSatisfied(schedule: Schedule): Long = elementsChecked(schedule) count check(schedule)

  def elementsChecked(schedule: Schedule): Seq[Checked]

  def check(schedule: Schedule)(checked: Checked): Boolean
}

object Preference {

  sealed abstract class Strength

  object Strong extends Strength

  object Weak extends Strength

}