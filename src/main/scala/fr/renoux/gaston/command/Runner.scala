package fr.renoux.gaston.command

import java.time.Instant

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine._
import fr.renoux.gaston.model.{Problem, Schedule}
import fr.renoux.gaston.util.Tools

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

/** Runs the whole schedule-searching stuff for a fixed amount of time. Can take hooks to do stuff at some frequency, like warn the user. */
class Runner(
    problem: Problem,
    engine: Engine,
    hook: (Schedule, Long) => Unit = (_, _) => (),
    hookFrequency: FiniteDuration = 20.seconds,
    parallelRunCount: Int = math.max(1, Runtime.getRuntime.availableProcessors * 2 / 3)
) {

  private implicit val _p: Problem = problem

  private val log = Logger[Runner]


  private val hookFrequencyMillis = hookFrequency.toMillis

  /** Produces a schedule and the associated score */
  def run(
      maxDuration: Option[FiniteDuration] = None,
      seed: Long = Random.nextLong()
  )(implicit tools: Tools): (Schedule, Long) = {

    val now = Instant.now()
    val timeout: Option[Instant] = maxDuration.map(d => now.plusSeconds(d.toSeconds))

    /* Parallelize on the number of cores */
    val future = Future.sequence {
      (0 until parallelRunCount).map { i =>
        implicit val random: Random = new Random(seed + i)
        Future {
          runRecursive(now, timeout, 0, Schedule.everyoneUnassigned)
        }
      }
    }

    val results: Seq[(Schedule, Long)] = Await.result(future, maxDuration.map(2 * _).getOrElse(Duration.Inf))
    results.fold((Schedule.everyoneUnassigned, 0L)) {
      case ((best, totalCount), (current, count)) =>
        if (best < current) (current, totalCount + count)
        else (best, totalCount + count)
    }
  }

  /** Recursive run, single-threaded: if it still has time, produces a schedule then invokes itself again . */
  @tailrec
  private def runRecursive(
      nextLog: Instant,
      timeout: Option[Instant],
      count: Long,
      current: Schedule
  )(implicit random: Random, tools: Tools): (Schedule, Long) = {
    val now = Instant.now()

    /* If time's out, stop now */
    if (timeout.exists(_ isBefore now)) {
      log.info(s"We have tried $count schedules ! It is time to stop !")
      (current, count)

    } else {
      /* If the last log is old enough, render the current best schedule */
      val newNextLog = if (now isAfter nextLog) {
        hook(current, count)
        Instant.now().plusMillis(hookFrequencyMillis)
      } else nextLog

      /* Run once then recurse */
      val ss = engine.run(random.nextLong)
      if (ss.score > current.score) runRecursive(newNextLog, timeout, count + 1, ss)
      else runRecursive(newNextLog, timeout, count + 1, current)
    }
  }

}
