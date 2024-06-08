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

/** Starts multiple Engines, with different seeds, and run them in parallel.
  *
  * It can function in two different modes, that can be combined:
  * - Run for a fixed amount of time, then aggregate the results together and returns the best one.
  * - Run continuously and regularly output the best result found.
  */
class Runner(
    startup: () => Unit = () => (),
    parallelism: Int = math.max(1, Runtime.getRuntime.availableProcessors - 1)
)(implicit problem: Problem, engine: Engine, output: Output, ctx: Context) {

  private val log = Logger[Runner]

  /** Interval of time between each status output */
  private val statusFrequency = 20.seconds.toMillis // TODO shouldn't be hardcoded

  /** Produces a schedule. Also returns the number of schedule examined */
  def run(
      seed: Long = Random.nextLong(),
      optimParams: OptimParams
  ): (Schedule, Long) = {

    val now = Instant.now()
    val maxDuration = optimParams.timeout.map(_ - now)

    /* Parallelize on the number of cores */
    val future: Future[Seq[(Schedule, Long)]] = Future.sequence {
      (0 until parallelism).map { i =>
        implicit val random: Random = new Random(seed + i)
        Future {
          startup()
          runRecursive(now.plusMillis(statusFrequency), 0, Schedule.abysmal)(optimParams)
        }
      }
    }

    val results: Seq[(Schedule, Long)] = Await.result(future, maxDuration.map(2 * _).getOrElse(Duration.Inf))
    results.reduceLeftOption[(Schedule, Long)] {
      case ((best, totalCount), (current, count)) =>
        if (best < current) (current, totalCount + count)
        else (best, totalCount + count)
    }.getOrElse(throw new Exception("Could not find a single schedule in the allotted time..."))
  }

  /** Recursive run, single-threaded: if it still has time, produces a schedule then invokes itself again . */
  @tailrec
  private def runRecursive(
      nextStatusTime: Instant,
      totalAttemptsCount: Long,
      best: Schedule,
      nextSchedules: LazyList[(Schedule, Long)] = LazyList.empty
  )(optimParams: OptimParams)(implicit random: Random): (Schedule, Long) = {
    val now = Instant.now()

    /* If time's out, stop now */
    if (optimParams.timeout.exists(_ isBefore now) || optimParams.maxIterations.exists(_ <= totalAttemptsCount)) {
      log.info(s"We have tried $totalAttemptsCount schedules ! It is time to stop !")
      (best, totalAttemptsCount)

    } else {
      /* If the last log is old enough, render the current best schedule */
      val newNextStatusTime = if (now isAfter nextStatusTime) {
        output.writeScheduleIfBetter(best)
        output.writeAttempts(totalAttemptsCount, best)
        Instant.now().plusMillis(statusFrequency)
      } else nextStatusTime

      /* Run once then recurse */

      nextSchedules match {
        case (ss: Schedule, attemptsCount: Long) #:: (tail: LazyList[(Schedule, Long)]) =>
          if (ss.score > best.score) runRecursive(newNextStatusTime, totalAttemptsCount + attemptsCount, ss, tail)(optimParams)
          else runRecursive(newNextStatusTime, totalAttemptsCount + attemptsCount, best, tail)(optimParams)
        case _ => // we finished the list
          output.writeNewScheduleChain()
          val newSeq = engine.lazySeq(random.nextLong(), optimParams)
          runRecursive(newNextStatusTime, totalAttemptsCount, best, newSeq)(optimParams)
      }
    }
  }

}
