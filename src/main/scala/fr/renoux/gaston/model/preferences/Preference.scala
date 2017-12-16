package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.Schedule

/**
  * The more preferences are satisfied the better, but it's not mandatory.
  */
trait Preference {
  def strength: Preference.Strength

  def countSatisfied(schedule: Schedule): Int
}



object Preference {

  sealed abstract class Strength

  object Strong extends Strength

  object Weak extends Strength

}