package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
  * Created by gael on 07/05/17.
  */
class PreferredScheduleFactorySpec extends FlatSpec with Matchers {
  val log = Logger[PreferredScheduleFactorySpec]

  import fr.renoux.gaston._

  "simpleAmelioration" should "work a valid random schedule (on a complex model)" in {
    val complexTestModel = ComplexTestModel(42L)
    var bestScore = Double.NegativeInfinity
    for (seed <- 0L until 5L) {
      implicit val random: Random = new Random(seed)
      log.info(s"Seed: $seed")

      val csFactory = new ConstrainedScheduleFactory(complexTestModel.Problems.Complete)
      val Some(initialSolution) = csFactory.makeSchedule
      val initialScore = complexTestModel.Problems.Complete.score(initialSolution)
      log.info(s"Temporary solution (score $initialScore): $initialSolution")

      val psFactory = new PreferredScheduleFactory(complexTestModel.Problems.Complete)
      val finalSolution = psFactory.simpleRandomizedAmelioration(initialSolution, initialScore, 10000)
      val finalScore = complexTestModel.Problems.Complete.score(finalSolution)
      log.info(s"Solution (score $finalScore): $finalSolution")

      complexTestModel.Problems.Complete.isSolvedBy(finalSolution) should be(true)
      finalScore should be > initialScore

      bestScore = math.max(bestScore, finalScore.value)
    }

    log.info(s"Bestest score was $bestScore")
  }

  "systematicAmelioration" should "work a valid schedule (on a complex model)" in {
    val complexTestModel = ComplexTestModel(42L)
    var bestScore = Double.NegativeInfinity
    for (seed <- 0L until 10L) {
      implicit val random: Random = new Random(seed)
      log.info(s"Seed: $seed")

      val csFactory = new ConstrainedScheduleFactory(complexTestModel.Problems.Complete)
      val Some(initialSolution) = csFactory.makeSchedule
      val initialScore = complexTestModel.Problems.Complete.score(initialSolution)
      log.info(s"Temporary solution (score $initialScore): $initialSolution")

      val psFactory = new PreferredScheduleFactory(complexTestModel.Problems.Complete)
      val finalSolution = psFactory.systematicAmelioration(initialSolution, initialScore, 1000)
      val finalScore = complexTestModel.Problems.Complete.score(finalSolution)
      log.info(s"Solution (score $finalScore): $finalSolution")

      complexTestModel.Problems.Complete.isSolvedBy(finalSolution) should be(true)
      finalScore should be > initialScore

      bestScore = math.max(bestScore, finalScore.value)
    }

    log.info(s"Bestest score was $bestScore")
  }

}
