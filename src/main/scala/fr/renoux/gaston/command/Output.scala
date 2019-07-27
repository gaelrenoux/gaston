package fr.renoux.gaston.command

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.ScheduleGenerator.BacktrackingFailures
import fr.renoux.gaston.input.{InputLoader, InputModel}
import fr.renoux.gaston.model.{Problem, Schedule, Score}

/** Destination of all information in Gaston */
class Output(silent: Boolean = false)(implicit val problem: Problem) {

  private val separatorLine = "*" * 80

  private val log = Logger[Output]
  private val notSilent = !silent

  private var bestScore: Score = Score.MinValue // scalastyle:ignore var.field

  private def write(txt: => String): Unit = {
    log.info(txt)
    if (notSilent) {
      println(s"$separatorLine\n$txt\n$separatorLine\n")
    }
  }

  def writeStart(seed: Long): Unit = write(s"Starting to run ! (seed #$seed)")

  def writeEnd(schedule: Schedule): Unit = {
    val render = new Renderer(problem)
    write(s"Finished !\n\n${render(schedule)}\n")
  }

  def writeInput(input: InputModel): Unit =
    write(s"\n${InputLoader.render(input)}\n")

  def writeScheduleIfBetter(schedule: Schedule): Unit = synchronized {
    if (schedule.score > bestScore) {
      bestScore = schedule.score
      val render = new Renderer(problem)
      write(render(schedule))
    }
  }

  def writeAttempts(count: Long): Unit = synchronized {
    write(s"We have tried $count schedules on thread ${Thread.currentThread().getName} !")
  }

  def writeBacktrackingFailure(fs: BacktrackingFailures): Unit = {
    if (fs.total % 50000 == 0) {
      val noTopicMessages = fs.noTopics.toSeq.map{
        case (slot, count) =>
          val percent = 100.0 * count / fs.total
          percent -> s"[${percent.round}%] Not enough topics on slot ${slot.name}"
      }
      val maxParaMessages = fs.maxParallelizationReached.toSeq.map{
        case (slot, count) =>
          val percent = 100.0 * count / fs.total
          percent -> s"[${percent.round}%] Max number of topics too low on slot ${slot.name}"
      }
      val allMessages = (noTopicMessages ++ maxParaMessages).sortBy(_._1).reverse.map(_._2).mkString("\n")

      write(s"I'm having trouble generating a valid schedule. Probable causes are: \n$allMessages")
    }
  }
}
