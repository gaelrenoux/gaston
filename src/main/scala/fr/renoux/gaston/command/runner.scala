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

  /** Same as `run`, and returns the best schedule found. Fails if no schedule could be generated. */
  final def runAndGetBest(termination: Termination): (Schedule, Long) = {
    val (bestSchedules, count) = run(termination)
    (bestSchedules.best.getOrElse(throw new Exception("Could not find a single schedule in the allotted time...")), count)
  }

  /** Generate schedules over time, outputting them at the statusFrequency interval, and keeping up to `keptCount` best
   * schedules, each from a different schedule chain. Will only terminate if some termination conditions are set. If it
   * terminates, its result is the `keptCount` (or fewer) best schedules found and the count of schedules examined. */
  def run(termination: Termination, keptCount: Int = 1): (BestSchedules, Long)
}


/** A SyncRunner simply runs in the current thread. */
class SyncRunner(seed: Long, statusDisplayInterval: FiniteDuration = 1.day)
  (using problem: Problem, engine: Engine, output: Output, ctx: Context)
  extends Runner(seed, statusDisplayInterval) {

  /** Runs the schedule generation. The argument controls when to stop the process. If no termination condition is set,
   * this method never terminates. */
  override def run(termination: Termination, keptCount: Int): (BestSchedules, Long) = {
    given Random = new Random(seed)
    output.writeStartThread(seed)
    runRecursive(Instant.now().plusMillis(statusDisplayIntervalMs), 0, 0, Schedule.abysmal, BestSchedules.empty(keptCount))(termination)
  }

  /** Recursive run, single-threaded: if it still has time, produces a schedule then invokes itself again . */
  @tailrec
  private def runRecursive(
      nextStatusTime: Instant,
      currentChainCount: Long,
      previousChainsCount: Long,
      best: Schedule,
      bestSchedules: BestSchedules,
      nextSchedules: LazyList[(Schedule, Long)] = LazyList.empty
  )(termination: Termination)(using random: Random): (BestSchedules, Long) = {
    val now = Instant.now()
    val totalCount = currentChainCount + previousChainsCount

    /* Check for termination criteria */
    if (termination.checkStopRequested()) {
      output.writeTerminationOnStopRequest()
      (bestSchedules, totalCount)

    } else if (termination.checkTimeout(now)) {
      output.writeTerminationOnTimeout(termination, now)
      (bestSchedules, totalCount)

    } else if (termination.checkCount(totalCount)) {
      output.writeTerminationOnCount(termination, totalCount)
      (bestSchedules, totalCount)

    } else if (termination.checkScore(best.score)) {
      output.writeTerminationOnScore(termination, best.score)
      (bestSchedules, totalCount)

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
          val newBest = if (ss.score > best.score) ss else best
          runRecursive(newNextStatusTime, attemptsCount, previousChainsCount, newBest, bestSchedules.add(ss), tail)(termination)
        case _ => // we finished the lazy-list, let's start a new one
          // TODO The part "new chain" should be in its own class, it's another responsibility than what the Runner is
          //  doing (although we'd need a way to mark a chain-change in the output).
          val newChainSeed = random.nextLong()
          output.writeNewScheduleChain(newChainSeed)
          val newSeq = engine.lazySeq(newChainSeed, termination.reduceCount(totalCount))
          runRecursive(newNextStatusTime, 0, totalCount, best, bestSchedules, newSeq)(termination)
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
  override def run(termination: Termination, keptCount: Int): (BestSchedules, Long) = {
    val now = Instant.now()
    val maxDuration = termination.timeout.map(_ - now)

    /* Parallelize on the number of cores */
    val future: Future[Seq[(BestSchedules, Long)]] = Future.sequence {
      (0 until parallelism).map { _ =>
        val runner = new SyncRunner(random.nextLong(), statusDisplayInterval)
        Future(runner.run(termination, keptCount))
      }
    }

    val results: Seq[(BestSchedules, Long)] = Await.result(future, maxDuration.map(2 * _).getOrElse(Duration.Inf))
    results.foldLeft((BestSchedules.empty(keptCount), 0L)) {
      case ((allBest, totalCount), (threadBest, count)) => (allBest.merge(threadBest), totalCount + count)
    }
  }

}

object ParallelRunner {
  private def defaultParallelism(): Int = math.max(1, Runtime.getRuntime.availableProcessors - 1)
}
