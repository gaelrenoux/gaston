package fr.renoux.gaston.engine

import fr.renoux.gaston.model.{Schedule, Score}
import fr.renoux.gaston.model.problem.Problem

/**
  * Scoring a schedule for a certain problem.
  */
object Scoring {

  /** Returns Left with the number (>0) of broken constraints, or Right with the score. */
  def score(problem: Problem, schedule: Schedule): Either[Int, Score] = {
    val constraintsBroken = problem.constraints.map(_.countBroken(schedule)).sum
    if (constraintsBroken > 0) Left(constraintsBroken)
    else {
      val preferenceScores = problem.preferences.map (_.score(schedule))
      Right(preferenceScores.sum)
    }
  }
}
