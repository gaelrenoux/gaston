package fr.renoux.gaston.command

import java.time.Instant

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine._
import fr.renoux.gaston.model.{Problem, Schedule}
import fr.renoux.gaston.util.Context

import scala.Ordering.Implicits._
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random
import fr.renoux.gaston.util.CanAddDuration._

/** Runs the whole schedule-searching stuff for a fixed amount of time. Can take hooks to do stuff at some frequency, like warn the user. */
class Runner(
    hook: (Schedule, Long) => Unit = (_, _) => (),
    hookFrequency: FiniteDuration = 20.seconds,
    parallelRunCount: Int = math.max(1, Runtime.getRuntime.availableProcessors - 1)
)(implicit problem: Problem, engine: Engine, ctx: Context) {

  private val log = Logger[Runner]

  private val hookFrequencyMillis = hookFrequency.toMillis

  /** Produces a schedule. Also returns the number of schedule examined */
  def run(
      seed: Long = Random.nextLong(),
      optimParams: OptimParams
  ): (Schedule, Long) = {

    val now = Instant.now()
    val maxDuration = optimParams.timeout.map(_ - now)

    /* Parallelize on the number of cores */
    val future: Future[Seq[(Schedule, Long)]] = Future.sequence {
      (0 until parallelRunCount).map { i =>
        implicit val random: Random = new Random(seed + i)
        Future {
          runRecursive(now.plusMillis(hookFrequencyMillis), 0, Schedule.everyoneUnassigned)(optimParams)
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
      nextHookInvocation: Instant,
      count: Long,
      current: Schedule,
      nextSchedules: LazyList[Schedule] = LazyList.empty
  )(optimParams: OptimParams)(implicit random: Random): (Schedule, Long) = {
    val now = Instant.now()

    /* If time's out, stop now */
    if (optimParams.timeout.exists(_ isBefore now) || optimParams.maxIterations.exists(_ <= count)) {
      log.info(s"We have tried $count schedules ! It is time to stop !")
      (current, count)

    } else {
      /* If the last log is old enough, render the current best schedule */
      val newNextLog = if (now isAfter nextHookInvocation) {
        hook(current, count)
        Instant.now().plusMillis(hookFrequencyMillis)
      } else nextHookInvocation

      /* Run once then recurse */

      val evaluated: LazyList[Schedule] = if (nextSchedules.nonEmpty) nextSchedules else {
        log.info("Starting a new schedule chain")
        engine.lazySeq(random.nextLong(), optimParams)
      }
      evaluated match {
        case (ss: Schedule) #:: (tail: LazyList[Schedule]) =>
          if (ss.score > current.score) runRecursive(newNextLog, count + 1, ss, tail)(optimParams)
          else runRecursive(newNextLog, count + 1, current, tail)(optimParams)
        case _ =>
          runRecursive(newNextLog, count + 1, current)(optimParams)
      }
    }
  }

}
