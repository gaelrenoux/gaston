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
  def run(termination: Termination): (Schedule, Long)
}


/** A SyncRunner simply runs in the current thread. */
class SyncRunner(seed: Long)(implicit problem: Problem, engine: Engine, output: Output, ctx: Context)
  extends Runner(seed) {

  private val log = Logger[SyncRunner]

  /** Runs the schedule generation. The argument controls when to stop the process. If no termination condition is set,
    * this method never terminates. */
  override def run(termination: Termination): (Schedule, Long) = {
    implicit val random: Random = new Random(seed)
    output.writeStartThread(seed)
    runRecursive(Instant.now().plusMillis(statusFrequencyMs), 0, 0, Schedule.abysmal)(termination)
  }

  /** Recursive run, single-threaded: if it still has time, produces a schedule then invokes itself again . */
  @tailrec
  private def runRecursive(
      nextStatusTime: Instant,
      currentChainCount: Long,
      previousChainsCount: Long,
      best: Schedule,
      nextSchedules: LazyList[(Schedule, Long)] = LazyList.empty
  )(termination: Termination)(implicit random: Random): (Schedule, Long) = {
    val now = Instant.now()
    val totalCount = currentChainCount + previousChainsCount

    /* Check for termination criteria */
    if (termination.checkTimeout(now)) {
      // TODO Log should go into Output as well
      log.info(s"Termination on timeout: $now > ${termination.timeout}")
      (best, totalCount)

    } else if (termination.checkCount(totalCount)) {
      log.info(s"Termination on count: $totalCount >= ${termination.count}")
      (best, totalCount)

    } else if (termination.checkScore(best.score)) {
      log.info(s"Termination on score: ${best.score} >= ${termination.score}")
      (best, totalCount)

    } else {
      /* If the last log is old enough, render the current best schedule */
      val newNextStatusTime = if (now isAfter nextStatusTime) {
        output.writeScheduleIfBetter(best)
        output.writeAttempts(totalCount, best)
        now.plusMillis(statusFrequencyMs)
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
class ParallelRunner(seed: Long = Random.nextLong(), parallelism: Int = ParallelRunner.defaultParallelism())
  (implicit problem: Problem, engine: Engine, output: Output, ctx: Context)
  extends Runner(seed) {

  /** Runs multiple Runners, in parallel, then wait for them to finish. Note that if no termination conditions are set,
    * this method will never terminate. */
  def run(termination: Termination): (Schedule, Long) = {
    val now = Instant.now()
    val maxDuration = termination.timeout.map(_ - now)

    /* Parallelize on the number of cores */
    val future: Future[Seq[(Schedule, Long)]] = Future.sequence {
      (0 until parallelism).map { _ =>
        val runner = new SyncRunner(random.nextLong())
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
