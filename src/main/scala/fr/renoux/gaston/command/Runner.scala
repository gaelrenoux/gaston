package fr.renoux.gaston.command

import java.time.Instant

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine._
import fr.renoux.gaston.model.{Problem, Schedule, Score}
import fr.renoux.gaston.util.Tools

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

/** Runs the whole schedule-searching stuff for a fixed amount of time. Can take hooks to do stuff at some frequency, like warn the user. */
class Runner(
                problem: Problem,
                improverConstructor: Problem => ScheduleImprover = new GreedyScheduleImprover(_),
                hook: (Schedule, Score, Long) => Unit = (_, _, _) => (),
                hookFrequency: FiniteDuration = 20.seconds
) {
  private val parallelRunCount: Int = math.max(1, Runtime.getRuntime.availableProcessors * 2 / 3)

  private implicit val _: Problem = problem

  private val log = Logger[Runner]

  private val generator = new ScheduleGenerator(problem)
  private val improver = improverConstructor(problem)
  private val engine = new Engine(generator, improver)

  private val hookFrequencyMillis = hookFrequency.toMillis

  /** Produces a schedule and the associated score */
  def run(
      maxDuration: Option[FiniteDuration] = None,
      seed: Long = Random.nextLong()
  )(implicit tools: Tools): (Schedule, Score, Long) = {

    val now = Instant.now()
    val timeout: Option[Instant] = maxDuration.map(d => now.plusSeconds(d.toSeconds))

    /* Parallelize on the number of cores */
    val future = Future.sequence {
      (0 until parallelRunCount).map { i =>
        implicit val random: Random = new Random(seed + i)
        Future {
          runRecursive(now, timeout, 0, Schedule.empty, Score.MinValue)
        }
      }
    }

    val results: Seq[(Schedule, Score, Long)] = Await.result(future, maxDuration.map(2 * _).getOrElse(Duration.Inf))
    results.fold((Schedule.empty, Score.Zero, 0L)) {
      case ((bestSchedule, bestScore, totalCount), (schedule, score, count)) =>
        if (score > bestScore) (schedule, score, totalCount + count)
        else (bestSchedule, bestScore, totalCount + count)
    }
  }

  /** Recursive run, single-threaded: if it still has time, produces a schedule then invokes itself again . */
  @tailrec
  private def runRecursive(
      lastLog: Instant,
      timeout: Option[Instant],
      count: Long,
      currentSchedule: Schedule,
      currentScore: Score
  )(implicit random: Random, tools: Tools): (Schedule, Score, Long) = {
    val now = Instant.now()

    /* If time's out, stop now */
    if (timeout.exists(_ isBefore now)) {
      log.info(s"We have tried $count schedules ! It is time to stop !")
      (currentSchedule, currentScore, count)

    } else {
      /* If the last log is old enough, render the current best schedule */
      val newLastLog = if (now isAfter lastLog.plusMillis(hookFrequencyMillis)) {
        hook(currentSchedule, currentScore, count)
        Instant.now()
      } else lastLog

      /* Run once then recurse */
      val (schedule, score) = engine.run(random.nextLong)
      if (score > currentScore) runRecursive(newLastLog, timeout, count + 1, schedule, score)
      else runRecursive(newLastLog, timeout, count + 1, currentSchedule, currentScore)
    }
  }

}
