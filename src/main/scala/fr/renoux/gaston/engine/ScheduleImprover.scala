package fr.renoux.gaston.engine

import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Schedule, Score}

import scala.util.Random

/**
  * Improves an existing Schedule by satisfying preferences.
  */
abstract class ScheduleImprover(problem: Problem) extends Scoring(problem) {

  def improve(schedule: Schedule, initialScore: Score, rounds: Int = 10000)(implicit rand: Random): Schedule

}
