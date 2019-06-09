package fr.renoux.gaston.command

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.input.{InputLoader, InputModel}
import fr.renoux.gaston.model.{Problem, Schedule, Score}

/** Destination of all information in Gaston */
class Output(silent: Boolean = false)(implicit val problem: Problem) {

  private val SeparatorLine = "*" * 80

  private val log = Logger[Output]
  private val notSilent = !silent

  private var bestScore: Score = Score.MinValue

  private def write(txt: => String): Unit = {
    log.info(txt)
    if (notSilent) {
      println(s"$SeparatorLine\n$txt\n$SeparatorLine\n")
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
}
