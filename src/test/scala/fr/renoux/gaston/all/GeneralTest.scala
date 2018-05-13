package fr.renoux.gaston.all

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.{ConstrainedScheduleFactory, PreferredScheduleFactory}
import fr.renoux.gaston.io.{InputLoader, InputSettings}
import fr.renoux.gaston.model.preferences.PersonTopicPreference
import fr.renoux.gaston.model.{Person, Schedule, Score}
import fr.renoux.gaston.util.MapImplicits._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
  * Created by gael on 07/05/17.
  */
class GeneralTest extends FlatSpec with Matchers {
  private val log = Logger[GeneralTest]
  private implicit val settings: InputSettings = InputLoader.fromClassPath.forceToInput.gaston.settings

  "Gaston" should "produce a good schedule on a real-life model" in {
    val problem = InputLoader.fromClassPath("udocon-application.conf").forceToModel
    log.info(s"Problem was read")

    var bestSchedule = Schedule(0)
    var bestScore = Double.NegativeInfinity

    val csFactory = new ConstrainedScheduleFactory(problem)
    val psFactory = new PreferredScheduleFactory(problem)

    for (seed <- 0L until 100L) {
      implicit val random: Random = new Random(seed)
      log.info(s"Starting to create solution for seed $seed")

      val Some(initialSolution) = csFactory.makeSchedule
      val initialScore = psFactory.score(initialSolution)
      log.info(s"Temporary solution (score $initialScore): ${initialSolution.toFormattedString}")

      val finalSolution = psFactory.systematicAmelioration(initialSolution, initialScore, 10000)
      val finalScore = psFactory.score(finalSolution)
      log.info(s"Solution (score $finalScore): ${finalSolution.toFormattedString}")

      problem.isSolvedBy(finalSolution) should be(true)
      finalScore should be > initialScore

      if (finalScore.value > bestScore) {
        bestScore = finalScore.value
        bestSchedule = finalSolution
      }
    }

    val minForBestScore = psFactory.scoreMinimumOnly(bestSchedule).value
    val sumForBestScore = psFactory.scoreSumOnly(bestSchedule).value
    log.info(s"Bestest score was $bestScore (min was $minForBestScore and sum was $sumForBestScore)")

    val weightedScoresByPerson = psFactory.weightedScoresByPerson(bestSchedule).mapValues(_.value)
    log.info(s"For each person, score is:\n${weightedScoresByPerson.toFormattedString}")

    /* For each person, preferences (strongs are marked with true) */
    val preferencesByPerson: Map[Person, Set[(PersonTopicPreference, Boolean)]] = problem.preferences collect {
      case p@PersonTopicPreference(_, _, settings.strongPreference) => (p, true)
      case p@PersonTopicPreference(_, _, settings.weakPreference) => (p, false)
    } groupBy (_._1.person)

    val satisfiedPreferencesByPerson = preferencesByPerson map { case (person, prefs) =>
      person -> prefs.filter(_._1.score(bestSchedule) > Score.Zero)
    }
    //log.info(s"\nSatisfied preferences for each person are:\n${satisfiedPreferencesByPerson.toFormattedString}")

    val satisfiedPreferencesCountByPerson = satisfiedPreferencesByPerson.map { case (person, preferences) =>
      person -> (preferences.count(_._2), preferences.count(!_._2))
    }
    log.info(s"\nFor each person, how many strong and weak constraints are satisfied:\n${satisfiedPreferencesCountByPerson.toFormattedString}")

    log.info(s"Bestest schedule was ${bestSchedule.toFormattedString}")
  }
}
