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
          runRecursive(now.plusMillis(statusFrequency), 0, Schedule.startingUnassignedOrForced)(optimParams)
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
      val newNextStatusTime = if (now isAfter nextStatusTime) {
        output.writeScheduleIfBetter(current)
        output.writeAttempts(count, current)
        Instant.now().plusMillis(statusFrequency)
      } else nextStatusTime

      /* Run once then recurse */

      val evaluated: LazyList[Schedule] = if (nextSchedules.nonEmpty) nextSchedules else {
        output.writeNewScheduleChain()
        engine.lazySeq(random.nextLong(), optimParams)
      }
      evaluated match {
        case (ss: Schedule) #:: (tail: LazyList[Schedule]) =>
          if (ss.score > current.score) runRecursive(newNextStatusTime, count + 1, ss, tail)(optimParams)
          else runRecursive(newNextStatusTime, count + 1, current, tail)(optimParams)
        case _ =>
          runRecursive(newNextStatusTime, count + 1, current)(optimParams)
      }
    }
  }

}
