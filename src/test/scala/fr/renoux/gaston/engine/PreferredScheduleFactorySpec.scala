package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.io.{InputLoader, InputSettings}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
  * Created by gael on 07/05/17.
  */
class PreferredScheduleFactorySpec extends FlatSpec with Matchers {
  private val log = Logger[PreferredScheduleFactorySpec]
  private implicit val settings: InputSettings = InputLoader.fromClassPath.forceToInput.gaston.settings
  private val ComplexTestModel = fr.renoux.gaston.ComplexTestModel(42L)(settings)

  "simpleAmelioration" should "work a valid random schedule (on a complex model)" in {
    import ComplexTestModel._
    var bestScore = Double.NegativeInfinity
    for (seed <- 0L until 5L) {
      implicit val random: Random = new Random(seed)
      log.info(s"Seed: $seed")

      val csFactory = new ConstrainedScheduleFactory(Problems.Complete)
      val psFactory = new PreferredScheduleFactory(Problems.Complete)

      val Some(initialSolution) = csFactory.makeSchedule
      val initialScore = psFactory.score(initialSolution)
      log.info(s"Temporary solution (score $initialScore): $initialSolution")

      val finalSolution = psFactory.simpleRandomizedAmelioration(initialSolution, initialScore, 1000)
      val finalScore = psFactory.score(finalSolution)
      log.info(s"Solution (score $finalScore): $finalSolution")

      Problems.Complete.isSolvedBy(finalSolution) should be(true)
      finalScore should be > initialScore

      bestScore = math.max(bestScore, finalScore.value)
    }

    log.info(s"Bestest score was $bestScore")
  }

  "systematicAmelioration" should "work a valid schedule (on a complex model)" in {
    import ComplexTestModel._
    var bestScore = Double.NegativeInfinity
    for (seed <- 0L until 5L) {
      implicit val random: Random = new Random(seed)
      log.info(s"Seed: $seed")

      val csFactory = new ConstrainedScheduleFactory(Problems.Complete)
      val psFactory = new PreferredScheduleFactory(Problems.Complete)

      val Some(initialSolution) = csFactory.makeSchedule
      val initialScore = psFactory.score(initialSolution)
      log.info(s"Temporary solution (score $initialScore): $initialSolution")

      val finalSolution = psFactory.systematicAmelioration(initialSolution, initialScore, 100)
      val finalScore = psFactory.score(finalSolution)
      log.info(s"Solution (score $finalScore): $finalSolution")

      Problems.Complete.isSolvedBy(finalSolution) should be(true)
      finalScore should be > initialScore

      bestScore = math.max(bestScore, finalScore.value)
    }

    log.info(s"Bestest score was $bestScore")
  }


  "systematicAmelioration" should "work a valid schedule (on a real-life model)" in {
    var bestScore = Double.NegativeInfinity
    for (seed <- 0L until 5L) {
      implicit val random = new Random(0L)

      val problem = InputLoader.fromClassPath("udocon-application.conf").forceToModel

      val csFactory = new ConstrainedScheduleFactory(problem)
      val psFactory = new PreferredScheduleFactory(problem)

      val Some(initialSolution) = csFactory.makeSchedule
      val initialScore = psFactory.score(initialSolution)
      log.info(s"Temporary solution (score $initialScore): $initialSolution")

      val finalSolution = psFactory.systematicAmelioration(initialSolution, initialScore, 100)
      val finalScore = psFactory.score(finalSolution)
      log.info(s"Solution (score $finalScore): $finalSolution")

      problem.isSolvedBy(finalSolution) should be(true)
      finalScore should be > initialScore

      bestScore = math.max(bestScore, finalScore.value)
    }

    log.info(s"Bestest score was $bestScore")
  }
}
