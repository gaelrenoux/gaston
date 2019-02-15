package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.Schedule
import fr.renoux.gaston.model.problem.Problem
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class ScheduleImproverAbstractSpec extends FlatSpec with Matchers {

  private val log = Logger(getClass)

  def runWith(improverConstructor: Problem => ScheduleImprover, problem: Problem, seeds: Traversable[Long]): (Schedule, Double) = {

    var (bestSchedule, bestScore) = (Schedule.empty, Double.NegativeInfinity)
    for (seed <- seeds) {
      implicit val random: Random = new Random(seed)
      log.info(s"Seed: $seed")

      val csFactory = new ConstrainedScheduleFactory(problem)
      val improver = improverConstructor(problem)

      val Some(initialSolution) = csFactory.makeSchedule
      val initialScore = Scorer.score(problem, initialSolution)
      log.info(s"Temporary solution (score $initialScore): $initialSolution")

      val finalSolution = improver.improve(initialSolution, initialScore, 100)
      val finalScore = Scorer.score(problem, finalSolution)
      log.info(s"Solution (score $finalScore): $finalSolution")

      problem.isSolvedBy(finalSolution) should be(true)
      finalScore should be > initialScore

      if (finalScore.value > bestScore) {
        bestScore = finalScore.value
        bestSchedule = finalSolution
      }
    }

    log.info(s"Bestest score was $bestScore")
    log.info(s"Bestest schedule was ${bestSchedule.toFormattedString}")
    (bestSchedule, bestScore)
  }
}