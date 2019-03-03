package fr.renoux.gaston.command

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.Score
import scalaz.NonEmptyList

class Output(silent: Boolean = false) {

  private val log = Logger[Output]
  private val notSilent = !silent

  private var bestScore: Score = Score.Zero

  private var attempts: Long = 0L

  def write(txt: => String): Unit = {
    log.info(txt)
    if (notSilent) {
      println(txt)
    }
  }

  def writeStart(): Unit = write(s"Starting to run !")

  def writeEnd(txt: => String): Unit = write(txt)

  def writeScheduleIfBetter(score: Score, scheduleText: => String): Unit = synchronized {
    if (score > bestScore) {
      bestScore = score
      write(scheduleText)
    }
  }
  def writeAttempts(count: Long): Unit = synchronized {
    attempts += count
    write(s"We have tried $attempts schedules !")
  }

  def writeErrors(msg: NonEmptyList[String]): Unit =
    write("Failed to run.\n" + msg.list.toList.mkString("\n"))
}
