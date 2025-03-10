package fr.renoux.gaston.command

import fr.renoux.gaston.engine.*
import fr.renoux.gaston.model.{Problem, Schedule}
import fr.renoux.gaston.util.CanAddDuration.given
import fr.renoux.gaston.util.Context

import java.time.Instant
import scala.Ordering.Implicits.*
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}
import scala.util.Random

/** A Runner is a class that will use an Engine to generate schedules over and over again. If an Engine-produced lazy
 * sequence finishes, it is the responsibility of the Runner to start a new one, using its own random generator to
 * make a new seed for the new sequence.
 *
 * Runners may be mutable, as they typically handle their own random generator, and are NOT thread-safe.
 */
abstract class Runner(seed: Long, statusDisplayInterval: FiniteDuration) {

  /** Interval of time between each status output */
  protected val statusDisplayIntervalMs: Long = statusDisplayInterval.toMillis

  protected given random: Random = new Random(seed)

  /** Generate schedules over time, outputting them at the statusFrequency interval. Will only terminate if some
   * termination conditions are set. If it terminates, its result is the best schedule found and the count of schedules
   * examined. */
  def run(termination: Termination): (Schedule, Long)
}


/** A SyncRunner simply runs in the current thread. */
class SyncRunner(seed: Long, statusDisplayInterval: FiniteDuration = 1.day)
  (using problem: Problem, engine: Engine, output: Output, ctx: Context)
  extends Runner(seed, statusDisplayInterval) {

  /** Runs the schedule generation. The argument controls when to stop the process. If no termination condition is set,
   * this method never terminates. */
  override def run(termination: Termination): (Schedule, Long) = {
    given Random = new Random(seed)
    output.writeStartThread(seed)
    runRecursive(Instant.now().plusMillis(statusDisplayIntervalMs), 0, 0, Schedule.abysmal)(termination)
  }

  /** Recursive run, single-threaded: if it still has time, produces a schedule then invokes itself again . */
  @tailrec
  private def runRecursive(
      nextStatusTime: Instant,
      currentChainCount: Long,
      previousChainsCount: Long,
      best: Schedule,
      nextSchedules: LazyList[(Schedule, Long)] = LazyList.empty
  )(termination: Termination)(using random: Random): (Schedule, Long) = {
    val now = Instant.now()
    val totalCount = currentChainCount + previousChainsCount

    /* Check for termination criteria */
    if (termination.checkTimeout(now)) {
      output.writeTerminationOnTimeout(termination, now)
      (best, totalCount)

    } else if (termination.checkCount(totalCount)) {
      output.writeTerminationOnCount(termination, totalCount)
      (best, totalCount)

    } else if (termination.checkScore(best.score)) {
      output.writeTerminationOnScore(termination, best.score)
      (best, totalCount)

    } else {
      /* If the last log is old enough, render the current best schedule */
      val newNextStatusTime = if (now > nextStatusTime) {
        output.writeScheduleIfBetter(best)
        output.writeAttempts(totalCount, best)
        now.plusMillis(statusDisplayIntervalMs)
      } else nextStatusTime

      /* Read one schedule from the lazy list then recur */

      nextSchedules match {
        case (ss: Schedule, attemptsCount: Long) #:: (tail: LazyList[(Schedule, Long)]) =>
          if (ss.score > best.score) runRecursive(newNextStatusTime, attemptsCount, previousChainsCount, ss, tail)(termination)
          else runRecursive(newNextStatusTime, attemptsCount, previousChainsCount, best, tail)(termination)
        case _ => // we finished the lazy-list, let's start a new one
          // TODO The part "new chain" should be in its own class, it's another responsibility than what the Runner is
          //  doing (although we'd need a way to mark a chain-change in the output).
          val newChainSeed = random.nextLong()
          output.writeNewScheduleChain(newChainSeed)
          val newSeq = engine.lazySeq(newChainSeed, termination.reduceCount(totalCount))
          runRecursive(newNextStatusTime, 0, totalCount, best, newSeq)(termination)
      }
    }
  }
}


/** Runs multiple parallel SyncRunners, with different seeds, in parallel, then aggregate the results if they terminate.
 * Note that termination conditions must be met on all parallel runners for this one to terminate. */
// TODO Find a way to terminate early as soon as a minimum score is reached, but not for other termination conditions.
class ParallelRunner(seed: Long = Random.nextLong(), parallelism: Int = ParallelRunner.defaultParallelism(), statusDisplayInterval: FiniteDuration = 1.day)
  (using problem: Problem, engine: Engine, output: Output, ctx: Context)
  extends Runner(seed, statusDisplayInterval) {

  /** Runs multiple Runners, in parallel, then wait for them to finish. Note that if no termination conditions are set,
   * this method will never terminate. */
  def run(termination: Termination): (Schedule, Long) = {
    val now = Instant.now()
    val maxDuration = termination.timeout.map(_ - now)

    /* Parallelize on the number of cores */
    val future: Future[Seq[(Schedule, Long)]] = Future.sequence {
      (0 until parallelism).map { _ =>
        val runner = new SyncRunner(random.nextLong(), statusDisplayInterval)
        Future(runner.run(termination))
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
