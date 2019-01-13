package fr.renoux.gaston.engine

import java.time.Instant

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Schedule, Score}

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.Random

/** Runs the whole schedule-searching stuff for a fixed amount of time. Can take hooks to do stuff at some frequency, like warn the user. */
class Runner(
    problem: Problem,
    hook: (Schedule, Score, Long) => Unit = (_, _, _) => (),
    hookFrequency: FiniteDuration = 20.seconds
) {

  private val log = Logger[Runner]

  private val scorer = Scorer.of(problem)
  val csFactory = new ConstrainedScheduleFactory(problem)
  val psFactory = new SystematicScheduleImprover(problem)

  private val hookFrequencyMillis = hookFrequency.toMillis

  /** Produces a schedule and the associated score */
  def run(
      maxDuration: Option[FiniteDuration] = None,
      seed: Long = Random.nextLong()
  ): (Schedule, Score) = {
    implicit val random: Random = new Random(seed)

    val now = Instant.now()
    val timeout: Option[Instant] = maxDuration.map(d => now.plusSeconds(d.toSeconds))

    val (schedule, score) = runRecursive(now, timeout, 0, Schedule(0), Score.MinValue)
    (schedule, score)
  }

  /** Recursive run: if it still has time, produces a schedule then invokes itself again . */
  @tailrec
  private def runRecursive(
      lastLog: Instant,
      timeout: Option[Instant],
      count: Long,
      currentSchedule: Schedule,
      currentScore: Score
  )(implicit random: Random): (Schedule, Score) = {
    val now = Instant.now()

    /* If time's out, stop now */
    if (timeout.exists(_ isBefore now)) {
      log.info(s"We have tried $count schedules ! It is time to stop !")
      (currentSchedule, currentScore)

    } else {
      /* If the last log is old enough, render the current best schedule */
      val newLastLog = if (now isAfter lastLog.plusMillis(hookFrequencyMillis)) {
        hook(currentSchedule, currentScore, count)
        Instant.now()
      } else lastLog

      /* Run once then recurse */
      val (schedule, score) = runOnce()
      if (score > currentScore) runRecursive(newLastLog, timeout, count + 1, schedule, score)
      else runRecursive(newLastLog, timeout, count + 1, currentSchedule, currentScore)
    }
  }

  /** Produces a schedule and its score */
  def runOnce()(implicit random: Random): (Schedule, Score) = {
    val Some(initialSolution) = csFactory.makeSchedule
    val initialScore = scorer.score(initialSolution)

    val finalSolution = psFactory.improve(initialSolution, initialScore, 10000)
    val finalScore = scorer.score(finalSolution)

    (finalSolution, finalScore)
  }


}
