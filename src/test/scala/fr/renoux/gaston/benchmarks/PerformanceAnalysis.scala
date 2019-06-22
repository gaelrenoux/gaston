package fr.renoux.gaston.benchmarks

import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.UdoConTestModel
import fr.renoux.gaston.command.Runner
import fr.renoux.gaston.engine._
import fr.renoux.gaston.input._
import fr.renoux.gaston.model.Problem
import fr.renoux.gaston.util.{Chrono, Tools}

import scala.concurrent.duration._

object PerformanceAnalysis extends App {

  private val udoConProblem = problemFromClassPath("udocon2017/uc17-completed.conf").force
  private val lastYear = UdoConTestModel.Solutions.Actual

  /* Check in case of modifications */
  val broken = udoConProblem.constraints.filter(!_.isRespected(lastYear))
  if (broken.nonEmpty) {
    throw new IllegalStateException(s"Constraints broken: $broken")
  }



  val tools: Tools = Tools(new Chrono(blocking = true))
  implicit val problem: Problem = udoConProblem
  implicit val context: Context = Context(tools = tools)

  val duration: FiniteDuration = 1.minutes
  val seed: Long = 0L

  val engine = new Engine(backtrackInitialSchedule = true)
  val runner = new Runner(engine, parallelRunCount = 1)

  val (schedule, count) = runner.run(Some(duration), seed = seed)

  println(s"${schedule.score} after $count iterations")
  println(s"Times: ${tools.chrono.timesPretty}")
  println(s"Counts: ${tools.chrono.countsPretty}")

}