package fr.renoux.gaston.engine

import fr.renoux.gaston.model.{Problem, Schedule, ScoredSchedule}
import fr.renoux.gaston.util.Tools

import scala.util.Random

class Engine(
    problem: Problem,
    altImprover: Problem => ScheduleImprover = new GreedyScheduleImprover(_)
) {

  val generator: PartialSchedulesGenerator = new PartialSchedulesGenerator(problem)
  val filler: PartialScheduleFiller = new PartialScheduleFiller(problem)
  val improver: ScheduleImprover = altImprover(problem)

  /** Produces a schedule and its score */
  def run(seed: Long)(implicit tools: Tools): ScoredSchedule = {
    implicit val _r: Random = new Random(seed)

    val Some(initialSolution) = tools.chrono("ConstrainedScheduleFactory.makeSchedule") {
      generateUnimproved
    }
    val initialScore = Scorer.score(initialSolution)

    val finalSolution = tools.chrono("ScheduleImprover.improve") {
      improver.improve(initialSolution, initialScore)
    }
    val finalScore = Scorer.score(finalSolution)

    ScoredSchedule(finalSolution, finalScore)
  }

  /** For testing purposes */
  def generateUnimproved(implicit random: Random, ctx: Context): Option[Schedule] = {
    generator.lazySeq.map(filler.fill).flatten.headOption
  }
}
