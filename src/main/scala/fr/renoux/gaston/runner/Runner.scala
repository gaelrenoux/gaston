package fr.renoux.gaston.runner

import java.text.DecimalFormat
import java.time.Instant

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.{ConstrainedScheduleFactory, SystematicScheduleImprover}
import fr.renoux.gaston.io.InputSettings
import fr.renoux.gaston.model.preferences.PersonTopicPreference
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Person, Schedule, Score}

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.Random

class Runner(
              settings: InputSettings,
              problem: Problem,
              maxDuration: Option[FiniteDuration] = None,
              silent: Boolean = false,
              verbose: Boolean = false,
              seed: Long = Random.nextLong()
            ) {


  private val log = Logger[Runner]


  val csFactory = new ConstrainedScheduleFactory(problem)
  val psFactory = new SystematicScheduleImprover(problem)

  val timeout: Option[Instant] = maxDuration.map(d => Instant.now().plusSeconds(d.toSeconds))

  implicit val random: Random = new Random(seed)

  private val LogFrequenceMillis = (20 seconds).toMillis

  private val enhancerIterations = 10000

  private val DecimalFormat = new DecimalFormat("000.00")


  def run(): (Schedule, Score) = {
    val (schedule, score) = runRecursive(Instant.now(), 0, Schedule(0), Score.MinValue)
    /* Print final result if needed */
    render(schedule, score)
    (schedule, score)
  }

  @tailrec
  private def runRecursive(lastLog: Instant, count: Long, currentSchedule: Schedule, currentScore: Score): (Schedule, Score) = {
    val now = Instant.now()

    if (timeout.exists(_ isBefore now)) {
      log.info(s"We have tried $count schedules ! It is time to stop !")
      (currentSchedule, currentScore)
    } else {
      val newLastLog = if (now isBefore lastLog.plusMillis(LogFrequenceMillis)) lastLog else {
        render(currentSchedule, currentScore)
        log.info(s"We have tried $count schedules !")
        Instant.now()
      }

      val (schedule, score) = runOnce()
      if (score > currentScore) runRecursive(newLastLog, count +1, schedule, score)
      else runRecursive(newLastLog, count +1, currentSchedule, currentScore)
    }
  }

  def runOnce() = {
    val Some(initialSolution) = csFactory.makeSchedule
    val initialScore = psFactory.score(initialSolution)

    val finalSolution = psFactory.improve(initialSolution, initialScore, 10000)
    val finalScore = psFactory.score(finalSolution)

    (finalSolution, finalScore)
  }

  def render(schedule: Schedule, score: Score): Unit = {
    log.info(s"Schedule is \n${
      schedule.toFormattedString
    }\n")
    log.info(s"Schedule score is $score")

    val weightedScoresByPerson = psFactory.weightedScoresByPerson(schedule)

    /* For each person, preferences (strongs are marked with true) */
    val preferencesByPerson: Map[Person, Set[(PersonTopicPreference, Boolean)]] = problem.preferences collect {
      case p@PersonTopicPreference(_, _, settings.strongPreference) => (p, true)
      case p@PersonTopicPreference(_, _, settings.weakPreference) => (p, false)
    } groupBy (_._1.person)

    val satisfiedPreferencesByPerson = preferencesByPerson map {
      case (person, prefs) =>
        person -> prefs.filter(_._1.score(schedule) > Score.Zero)
    }
    val summaryByPerson = satisfiedPreferencesByPerson.map {
      case (person, preferences) =>
        person.name -> (weightedScoresByPerson(person), preferences.count(_._2), preferences.count(!_._2))
    }

    val summaryText = summaryByPerson.toSeq.sortBy(_._2._1).reverse map { case (name, (s, strong, weak)) =>
      val nameTxt = name.padTo(8, ' ').take(8)
      val scoreTxt = DecimalFormat.format(s.value)
      val strongTxt = f"$strong%2d"
      val weakTxt = f"$weak%2d"
      s"$nameTxt    $scoreTxt    $strongTxt  $weakTxt"
    } mkString "\n"
    log.info(s"\nPerson      Score     Str Weak\n$summaryText")
  }


}
