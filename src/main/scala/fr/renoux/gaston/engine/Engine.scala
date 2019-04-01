package fr.renoux.gaston.engine

import fr.renoux.gaston.model.ScoredSchedule
import fr.renoux.gaston.util.Tools

import scala.util.Random

class Engine(
    generator: ScheduleGenerator,
    improver: ScheduleImprover
) {

  /** Produces a schedule and its score */
  def run(seed: Long)(implicit tools: Tools): ScoredSchedule = {
    implicit val random = new Random(seed)

    val Some(initialSolution) = tools.chrono("ConstrainedScheduleFactory.makeSchedule") {
      generator.generate
    }
    val initialScore = Scorer.score(initialSolution)

    val finalSolution = tools.chrono("ScheduleImprover.improve") {
      improver.improve(initialSolution, initialScore)
    }
    val finalScore = Scorer.score(finalSolution)

    ScoredSchedule(finalSolution, finalScore)
  }

}
