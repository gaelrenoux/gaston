package fr.renoux.gaston.engine

import fr.renoux.gaston.model.Schedule
import fr.renoux.gaston.model.preferences.Preference
import fr.renoux.gaston.model.problem.Problem

/**
  * Scoring a schedule for a certain problem.
  */
object Scoring {
  val PreferencesScore = Map(Preference.Strong -> 10, Preference.Weak -> 1)

  /** Returns Left with the number (>0) of broken constraints, or Right with the score. */
  def score(problem: Problem, schedule: Schedule): Either[Int, Int] = {
    val constraintsBroken = problem.constraints.map(_.countBroken(schedule)).sum
    if (constraintsBroken > 0) Left(constraintsBroken)
    else {
      val score = problem.preferences map { pref =>
        pref.countSatisfied(schedule) * PreferencesScore(pref.strength)
      }
      Right(score.sum)
    }
  }
}
