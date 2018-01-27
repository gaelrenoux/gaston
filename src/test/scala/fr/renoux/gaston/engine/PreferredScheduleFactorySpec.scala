package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.{Settings, UdoConTestModel}
import fr.renoux.gaston.io.{Input, UdoConTableReader}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
  * Created by gael on 07/05/17.
  */
class PreferredScheduleFactorySpec extends FlatSpec with Matchers {
  val log = Logger[PreferredScheduleFactorySpec]
  implicit val settings: Settings = Input.fromClassPath._2
  val ComplexTestModel = fr.renoux.gaston.ComplexTestModel(42L)(settings)

  "simpleAmelioration" should "work a valid random schedule (on a complex model)" in {
    import ComplexTestModel._
    var bestScore = Double.NegativeInfinity
    for (seed <- 0L until 5L) {
      implicit val random: Random = new Random(seed)
      log.info(s"Seed: $seed")

      val csFactory = new ConstrainedScheduleFactory(Problems.Complete)
      val Some(initialSolution) = csFactory.makeSchedule
      val initialScore = Problems.Complete.score(initialSolution)
      log.info(s"Temporary solution (score $initialScore): $initialSolution")

      val psFactory = new PreferredScheduleFactory(Problems.Complete)
      val finalSolution = psFactory.simpleRandomizedAmelioration(initialSolution, initialScore, 1000)
      val finalScore = Problems.Complete.score(finalSolution)
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
      val Some(initialSolution) = csFactory.makeSchedule
      val initialScore = Problems.Complete.score(initialSolution)
      log.info(s"Temporary solution (score $initialScore): $initialSolution")

      val psFactory = new PreferredScheduleFactory(Problems.Complete)
      val finalSolution = psFactory.systematicAmelioration(initialSolution, initialScore, 100)
      val finalScore = Problems.Complete.score(finalSolution)
      log.info(s"Solution (score $finalScore): $finalSolution")

      Problems.Complete.isSolvedBy(finalSolution) should be(true)
      finalScore should be > initialScore

      bestScore = math.max(bestScore, finalScore.value)
    }

    log.info(s"Bestest score was $bestScore")
  }

  /*
  "generateRandomSolution" should "work a valid udocon schedule" in {
    implicit val random = new Random(0L)

    val csFactory = new ConstrainedScheduleFactory(UdoConTestModel.problem)
    val partialSolution = csFactory.makePartialSchedule
    log.debug(s"Partial solution: $partialSolution")

    val psFactory = new PreferredScheduleFactory(UdoConTestModel.problem)
    val tempSolution = psFactory.completePartialSchedule(partialSolution.get)
    log.debug(s"Temporary solution: $tempSolution")
    val finalSolution = psFactory.improveUntilItChecks(tempSolution)
    log.debug(s"Solution: $finalSolution")

    UdoConTestModel.problem.isSolved(finalSolution) should be(true)
  } */

}
