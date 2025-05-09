package fr.renoux.gaston.command

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.RandomScheduleGenerator.BacktrackingFailures
import fr.renoux.gaston.engine.Termination
import fr.renoux.gaston.input.{InputModel, InputRenderer}
import fr.renoux.gaston.model.{Problem, Schedule, Score}

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}
import scala.math.Ordering.Implicits.infixOrderingOps

/** Destination of all information in Gaston. It writes stuff both to the log file and the standard output. It has some
 * business-aware method, to make sure it only prints new schedules if they are better than schedules found
 * previously.
 *
 * It is thread-safe and can be used by all runners concurrently.
 */
final class Output private(silent: Boolean)(using val problem: Problem) {

  import Ordering.Double.IeeeOrdering

  private val separatorLine = "*" * 80

  private val log = Logger[Output]
  private val notSilent = !silent

  private val startMs = System.currentTimeMillis()

  private var bestScore: Score = Score.MinValue

  private val timeFormat = DateTimeFormatter.ofPattern("HH:mm:ss").withZone(ZoneOffset.UTC)

  private def time = {
    timeFormat.format(Instant.ofEpochMilli(System.currentTimeMillis() - startMs))
  }

  private def shortString(l: Long) = {
    if (l < 10000) l.toString
    else if (l < 10000000) s"${l / 1000}K"
    else if (l < 10000000000L) s"${l / 1000000}M"
    else s"${l / 1000000000L}G"
  }

  private def write(txt: => String, separator: Boolean = false): Unit = {
    log.info(txt)
    if (notSilent) {
      synchronized {
        if (separator) println(s"\n$separatorLine\n$time: $txt\n$separatorLine\n")
        else println(s"$time: $txt")
      }
    }
  }

  def writeStart(globalSeed: Long): Unit = write(s"Starting with global-seed: $globalSeed", separator = true)

  def writeStartThread(threadSeed: Long): Unit = write(s"Starting on thread ${Thread.currentThread().getName} with thread-seed: $threadSeed")

  def writeEnd(schedule: Schedule): Unit = {
    val render = new Renderer(problem)
    write(s"Finished !\n${render(schedule)}", separator = true)
  }

  def writeInput(input: InputModel): Unit =
    write(s"\n${InputRenderer.render(input)}\n")

  def writeScheduleIfBetter(schedule: Schedule): Unit = synchronized {
    if (schedule.score > bestScore) {
      bestScore = schedule.score
      val render = new Renderer(problem)
      write(s"From thread ${Thread.currentThread().getName}:\n${render(schedule)}", separator = true)
    }
  }

  def writeAttempts(count: Long, threadBestSchedule: Schedule): Unit = synchronized {
    val currentThread = Thread.currentThread()
    write(s"We have tried ${shortString(count)} schedules on thread ${currentThread.getName} (best score on thread is ${threadBestSchedule.score.value})")
  }

  def writeNewScheduleChain(chainSeed: Long): Unit = synchronized {
    write(s"Starting new schedule chain on thread ${Thread.currentThread().getName} with chain-seed: $chainSeed")
  }

  def writeBacktrackingFailure(fs: BacktrackingFailures): Unit = {
    if (fs.total % 200000 == 0) {
      val noTopicMessages = fs.noTopics.toSeq.map {
        case (slot, count) =>
          val percent = 100.0 * count / fs.total
          percent -> s"[${percent.round}%] Not enough topics on slot ${slot.name}"
      }
      val maxParaMessages = fs.maxParallelizationReached.toSeq.map {
        case (slot, count) =>
          val percent = 100.0 * count / fs.total
          percent -> s"[${percent.round}%] Max number of topics too low on slot ${slot.name}"
      }
      val allMessages = (noTopicMessages ++ maxParaMessages).sortBy(_._1).reverseIterator.map(_._2).mkString("\n")

      write(s"I'm having trouble generating a valid schedule (on thread ${Thread.currentThread().getName}). Probable causes are: \n$allMessages", separator = true)
    }
  }

  def writeTerminationOnTimeout(termination: Termination, now: Instant): Unit =
    write(s"Termination on timeout: $now > ${termination.timeout.get} on thread ${Thread.currentThread().getName}")

  def writeTerminationOnCount(termination: Termination, count: Long): Unit =
    write(s"Termination on count: $count >= ${termination.count.get} on thread ${Thread.currentThread().getName}")

  def writeTerminationOnScore(termination: Termination, score: Score): Unit = {
    write(s"Termination on score: ${score.value} >= ${termination.score.get.value} on thread ${Thread.currentThread().getName}")
  }
}

object Output {
  def apply(silent: Boolean = false)(using problem: Problem): Output = new Output(silent)

  def standard(using problem: Problem): Output = new Output(silent = false)

  def silent(using problem: Problem): Output = new Output(silent = true)
}
