package fr.renoux.gaston.command

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.input.{InputErrors, InputLoader, InputRoot, InputSettings}
import fr.renoux.gaston.model.{Problem, Score, ScoredSchedule}

/** Destination of all information in Gaston */
class Output(silent: Boolean = false) {

  private val SeparatorLine = "*" * 80

  private val log = Logger[Output]
  private val notSilent = !silent

  private var bestScore: Score = Score.Zero

  private def write(txt: => String): Unit = {
    log.info(txt)
    if (notSilent) {
      println(s"$SeparatorLine\n$txt\n$SeparatorLine\n")
    }
  }

  def writeStart(): Unit = write(s"Starting to run !")

  def writeEnd(scoredSchedule: ScoredSchedule, problem: Problem, settings: InputSettings): Unit = {
    val render = new Renderer(settings, problem)
    write(s"Finished !\n\n${render(scoredSchedule)}\n")
  }

  def writeInput(inputRoot: InputRoot): Unit =
    write(s"Aggregated configuration file is:\n\n${InputLoader.render(inputRoot)}\n")

  def writeScheduleIfBetter(scoredSchedule: ScoredSchedule, problem: Problem, settings: InputSettings): Unit = synchronized {
    if (scoredSchedule.score > bestScore) {
      bestScore = scoredSchedule.score
      val render = new Renderer(settings, problem)
      write(render(scoredSchedule))
    }
  }

  def writeAttempts(count: Long): Unit = synchronized {
    write(s"We have tried $count schedules on thread ${Thread.currentThread().getName} !")
  }

  def writeErrors(msg: InputErrors): Unit =
    write(s"Failed to run.\n${msg.list.toList.mkString("\n")}\n")
}
