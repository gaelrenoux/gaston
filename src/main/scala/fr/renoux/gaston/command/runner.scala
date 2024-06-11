package fr.renoux.gaston.command

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine._
import fr.renoux.gaston.model.{Problem, Schedule}
import fr.renoux.gaston.util.CanAddDuration._
import fr.renoux.gaston.util.Context

import java.time.Instant
import scala.Ordering.Implicits._
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

/** A Runner is a class that will use an Engine to generate schedules over and over again. If an Engine-produced lazy
  * sequence finishes, it is the responsibility of the Runner to start a new one, using its own random generator to
  * make a new seed for the new sequence.
  *
  * Runners may be mutable, as they typically handle their own random generator, and are NOT thread-safe.
  */
abstract class Runner(seed: Long) {

  /** Interval of time between each status output */
  protected val statusFrequencyMs: Long = 20.seconds.toMillis // TODO shouldn't be hardcoded

  protected implicit val random: Random = new Random(seed)

  /** Generate schedules over time, outputting them at the statusFrequency interval. Will only terminate if some
    * termination conditions are set. If it terminates, its result is the best schedule found and the count of schedules
    * examined. */
  def run(optimParams: OptimParams): (Schedule, Long)
}


/** A SyncRunner simply runs in the current thread. */
class SyncRunner(seed: Long)(implicit problem: Problem, engine: Engine, output: Output, ctx: Context)
  extends Runner(seed) {

  private val log = Logger[SyncRunner]

  /** Runs the schedule generation. The argument controls when to stop the process. If no termination condition is set,
    * this method never terminates. */
  override def run(optimParams: OptimParams): (Schedule, Long) = {
    implicit val random: Random = new Random(seed)
    output.writeStartThread(seed)
    runRecursive(Instant.now().plusMillis(statusFrequencyMs), 0, Schedule.abysmal)(optimParams)
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
        Instant.now().plusMillis(statusFrequencyMs)
      } else nextStatusTime

      /* Run once then recur */

      nextSchedules match {
        case (ss: Schedule, attemptsCount: Long) #:: (tail: LazyList[(Schedule, Long)]) =>
          if (ss.score > best.score) runRecursive(newNextStatusTime, totalAttemptsCount + attemptsCount, ss, tail)(optimParams)
          else runRecursive(newNextStatusTime, totalAttemptsCount + attemptsCount, best, tail)(optimParams)
        case _ => // we finished the list
          val newChainSeed = random.nextLong()
          output.writeNewScheduleChain(newChainSeed)
          val newSeq = engine.lazySeq(newChainSeed, optimParams)
          runRecursive(newNextStatusTime, totalAttemptsCount, best, newSeq)(optimParams)
      }
    }
  }
}


/** Runs multiple parallel SyncRunners, with different seeds, in parallel, then aggregate the results if they terminate. */
class ParallelRunner(seed: Long = Random.nextLong(), parallelism: Int = ParallelRunner.defaultParallelism())
  (implicit problem: Problem, engine: Engine, output: Output, ctx: Context)
  extends Runner(seed) {

  /** Runs multiple Runners, in parallel, then wait for them to finish. Note that if no termination conditions are set,
    * this method will never terminate. */
  def run(optimParams: OptimParams): (Schedule, Long) = {
    val now = Instant.now()
    val maxDuration = optimParams.timeout.map(_ - now)

    /* Parallelize on the number of cores */
    val future: Future[Seq[(Schedule, Long)]] = Future.sequence {
      (0 until parallelism).map { _ =>
        val runner = new SyncRunner(random.nextLong())
        Future(runner.run(optimParams))
      }
    }

    val results: Seq[(Schedule, Long)] = Await.result(future, maxDuration.map(2 * _).getOrElse(Duration.Inf))
    results.reduceLeftOption[(Schedule, Long)] {
      case ((best, totalCount), (current, count)) =>
        if (best < current) (current, totalCount + count)
        else (best, totalCount + count)
    }.getOrElse(throw new Exception("Could not find a single schedule in the allotted time..."))
  }

}

object ParallelRunner {
  private def defaultParallelism(): Int = math.max(1, Runtime.getRuntime.availableProcessors - 1)
}
