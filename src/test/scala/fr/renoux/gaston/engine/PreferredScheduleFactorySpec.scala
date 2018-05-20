package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.io.{InputLoader, InputSettings}
import fr.renoux.gaston.model.Schedule
import fr.renoux.gaston.model.constraints.TopicNeedsNumberOfPersons
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
      val psFactory = new RandomScheduleImprover(Problems.Complete)

      val Some(initialSolution) = csFactory.makeSchedule
      val initialScore = psFactory.score(initialSolution)
      log.info(s"Temporary solution (score $initialScore): $initialSolution")

      val finalSolution = psFactory.improve(initialSolution, initialScore, 1000)
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
      val psFactory = new SystematicScheduleImprover(Problems.Complete)

      val Some(initialSolution) = csFactory.makeSchedule
      val initialScore = psFactory.score(initialSolution)
      log.info(s"Temporary solution (score $initialScore): $initialSolution")

      val finalSolution = psFactory.improve(initialSolution, initialScore, 100)
      val finalScore = psFactory.score(finalSolution)
      log.info(s"Solution (score $finalScore): $finalSolution")

      Problems.Complete.isSolvedBy(finalSolution) should be(true)
      finalScore should be > initialScore

      bestScore = math.max(bestScore, finalScore.value)
    }

    log.info(s"Bestest score was $bestScore")
  }


  "systematicAmelioration" should "work a valid schedule (on a real-life model)" in {
    var (bestSchedule, bestScore) = (Schedule(0), Double.NegativeInfinity)
    for (seed <- 0L until 1L) {
      implicit val random = new Random(0L)

      val problem = InputLoader.fromClassPath("application.conf").forceToModel
      val problematicConstraints = problem.constraints collect {
        case c@TopicNeedsNumberOfPersons(_, _, max) if max < 6 => c
      }
      log.info("problematicConstraints:\n" + problematicConstraints.mkString("\n"))
      /* val problem = oldProblem.copy(
        constraints = oldProblem.constraints map {
          case TopicNeedsNumberOfPersons(t, min, max) => TopicNeedsNumberOfPersons(t , min, math.max(max, 6))
          case c => c
        }
      ) */

      val csFactory = new ConstrainedScheduleFactory(problem)
      val psFactory = new SystematicScheduleImprover(problem)

      val Some(initialSolution) = csFactory.makeSchedule
      val initialScore = psFactory.score(initialSolution)
      log.info(s"Temporary solution (score $initialScore): ${initialSolution.toFormattedString}")

      val finalSolution = psFactory.improve(initialSolution, initialScore, 100)
      val finalScore = psFactory.score(finalSolution)
      log.info(s"Solution (score $finalScore): ${finalSolution.toFormattedString}")

      problem.isSolvedBy(finalSolution) should be(true)
      finalScore should be > initialScore

      if (finalScore.value > bestScore) {
        bestScore = finalScore.value
        bestSchedule = finalSolution
      }
    }

    log.info(s"Bestest score was $bestScore")
    log.info(s"Bestest schedule was ${bestSchedule.toFormattedString}")
  }
}
