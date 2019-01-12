package fr.renoux.gaston.benchmarks

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.UdoConTestModel
import fr.renoux.gaston.engine.{ConstrainedScheduleFactory, Scorer, SystematicScheduleImprover}
import fr.renoux.gaston.io.PureConfigLoader
import fr.renoux.gaston.model.Schedule
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
  * Target is about 20 to 40. Score should be above 2900.
  */
class OldBenchmark extends FlatSpec with Matchers {
  private val log = Logger[OldBenchmark]

  "Gaston" should "benchmark" in {
    val problem = PureConfigLoader.fromClassPath("application.conf").forceToModel

    var bestSchedule = Schedule(0)
    var bestScore = Double.NegativeInfinity

    val scorer = new Scorer(problem)
    val csFactory = new ConstrainedScheduleFactory(problem)
    val psFactory = new SystematicScheduleImprover(problem, scorer)

    val lastYear = UdoConTestModel.Solutions.Actual
    problem.constraints.filter(!_.isRespected(lastYear)).foreach(c => log.info(s"Constraint broken $c"))

    var seed = 0
    val now = System.currentTimeMillis()

    while (System.currentTimeMillis() - now < 60000) {
      seed = seed + 1

      implicit val random: Random = new Random(seed)

      val Some(initialSolution) = csFactory.makeSchedule
      val initialScore = scorer.score(initialSolution)


      val finalSolution = psFactory.improve(initialSolution, initialScore, 10000)
      val finalScore = scorer.score(finalSolution)

      problem.isSolvedBy(finalSolution) should be(true)
      finalScore should be > initialScore

      if (finalScore.value > bestScore) {
        bestScore = finalScore.value
        bestSchedule = finalSolution
      }
    }

    log.info(s"Done $seed schedules and best score is $bestScore")
    bestScore should be > 2900.0
    seed should be > 20
  }
}
