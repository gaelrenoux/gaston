package fr.renoux.gaston.engine

import fr.renoux.gaston.model.{Problem, Schedule, Score}
import fr.renoux.gaston.util.Tools

import scala.util.Random

/**
  * Improves an existing Schedule by satisfying preferences.
  */
trait ScheduleImprover {
  val problem: Problem

  def improve(schedule: Schedule, initialScore: Score, rounds: Int = 10000)(implicit rand: Random, tools: Tools): Schedule
}
