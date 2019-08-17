package fr.renoux.gaston.benchmarks

import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.command.Runner
import fr.renoux.gaston.engine._
import fr.renoux.gaston.input._
import fr.renoux.gaston.model.Problem
import fr.renoux.gaston.util.{Chrono, Tools}

import scala.concurrent.duration._

object PerformanceAnalysis extends App {

  private val udoConProblem = problemFromClassPath("udocon2019/uc19-full.conf").force
  // TODO check a solution
  // 140+ iterations, 850+ score

  val tools: Tools = Tools(new Chrono(blocking = true))
  implicit val problem: Problem = udoConProblem
  implicit val context: Context = Context(tools = tools)

  val duration: FiniteDuration = 5.minutes
  val seed: Long = 0L

  val engine = new Engine(backtrackInitialSchedule = true)
  val runner = new Runner(engine)

  val (schedule, count) = tools.chrono("Total") {
    runner.run(Some(duration), seed = seed)
  }

  println(s"${schedule.score} after $count iterations")
  println(s"Times: ${tools.chrono.timesPretty}")
  println(s"Counts: ${tools.chrono.countsPretty}")

}