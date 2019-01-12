package fr.renoux.gaston.all

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.UdoConTestModel
import fr.renoux.gaston.engine.{ConstrainedScheduleFactory, Scoring, SystematicScheduleImprover}
import fr.renoux.gaston.io.{InputSettings, PureConfigLoader}
import fr.renoux.gaston.model.preferences.PersonTopicPreference
import fr.renoux.gaston.model.{Person, Schedule, Score}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
  * Created by gael on 07/05/17.
  */
class GeneralTest extends FlatSpec with Matchers {
  private val log = Logger[GeneralTest]
  private implicit val settings: InputSettings = PureConfigLoader.fromClassPath.forceToInput.gaston.settings

  "Gaston" should "produce a good schedule on a real-life model" ignore {
    val problem = PureConfigLoader.fromClassPath("application.conf").forceToModel
    log.info(s"Problem was read")

    var bestSchedule = Schedule(0)
    var bestScore = Double.NegativeInfinity

    val csFactory = new ConstrainedScheduleFactory(problem)
    val psFactory = new SystematicScheduleImprover(problem)

    val lastYear = UdoConTestModel.Solutions.Actual
    log.info(s"Was solved last year: " + problem.isSolvedBy(lastYear))
    problem.constraints.filter(!_.isRespected(lastYear)).foreach(c => log.info(s"Constraint broken $c"))


    for (seed <- 0L until 200L) {
      implicit val random: Random = new Random(seed)
      log.info(s"Starting to create solution for seed $seed")

      val Some(initialSolution) = csFactory.makeSchedule
      val initialScore = psFactory.score(initialSolution)
      log.info(s"Temporary solution (score $initialScore): ${initialSolution.toFormattedString}")

      val finalSolution = psFactory.improve(initialSolution, initialScore, 10000)
      val finalScore = psFactory.score(finalSolution)
      log.info(s"Solution (score $finalScore): ${finalSolution.toFormattedString}")

      problem.isSolvedBy(finalSolution) should be(true)
      finalScore should be > initialScore

      if (finalScore.value > bestScore) {
        bestScore = finalScore.value
        bestSchedule = finalSolution
      }
    }

    val Scoring.Detail(_, minForBestScore, sumForBestScore) = psFactory.scoreDetailed(bestSchedule)
    val Scoring.Detail(scoreLastYear, minForScoreLastYear, sumForScoreLastYear) = psFactory.scoreDetailed(lastYear)
    log.info(s"Bestest score was $bestScore (min was ${minForBestScore.value} and sum was ${sumForBestScore.value}) " +
      s"(actual schedule applied last year scored at ${scoreLastYear.value}, with min ${minForScoreLastYear.value} and sum ${sumForScoreLastYear.value})")

    val weightedScoresByPerson = psFactory.weightedScoresByPerson(bestSchedule)
    val weightedScoresByPersonLastYear = psFactory.weightedScoresByPerson(lastYear)
    val weightedScoresText = problem.persons map { p =>
      val name = p.name.padTo(8, ' ').take(8)
      val ps = weightedScoresByPerson(p)
      val ps2 = weightedScoresByPersonLastYear(p)
      s"$name: ${ps.toFormattedString} / ${ps2.toFormattedString}"
    } mkString "\n"
    log.info(s"For each person, scores are:\n$weightedScoresText")

    /* For each person, preferences (strongs are marked with true) */
    val preferencesByPerson: Map[Person, Set[(PersonTopicPreference, Boolean)]] = problem.preferences collect {
      case p@PersonTopicPreference(_, _, settings.strongPreference) => (p, true)
      case p@PersonTopicPreference(_, _, settings.weakPreference) => (p, false)
    } groupBy (_._1.person)

    val satisfiedPreferencesByPerson = preferencesByPerson map { case (person, prefs) =>
      person -> prefs.filter(_._1.score(bestSchedule) > Score.Zero)
    }
    val satisfiedPreferencesByPersonLastYear = preferencesByPerson map { case (person, prefs) =>
      person -> prefs.filter(_._1.score(lastYear) > Score.Zero)
    }

    val satisfiedPreferencesCountByPerson = satisfiedPreferencesByPerson.map { case (person, preferences) =>
      person.name -> (preferences.count(_._2), preferences.count(!_._2))
    }
    val satisfiedPreferencesCountByPersonLastYear = satisfiedPreferencesByPersonLastYear.map { case (person, preferences) =>
      person.name -> (preferences.count(_._2), preferences.count(!_._2))
    }
    val satisfiedPreferencesCountText = problem.persons map { p =>
      val name = p.name.padTo(8, ' ').take(8)
      val (strong, weak) = satisfiedPreferencesCountByPerson(p.name)
      val (strongLy, weakLy) = satisfiedPreferencesCountByPersonLastYear(p.name)
      s"$name: strongs $strong / $strongLy ; weaks  $weak / $weakLy"
    } mkString "\n"
    log.info(s"\nFor each person, how many strong and weak constraints are satisfied:\n$satisfiedPreferencesCountText")

    log.info(s"Bestest schedule was ${bestSchedule.toFormattedString}")
  }
}
