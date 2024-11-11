package fr.renoux.gaston.benchmarks

import fr.renoux.gaston.TestUtils.*
import fr.renoux.gaston.command.{Output, ParallelRunner}
import fr.renoux.gaston.engine.{Engine, GreedyEngine, Termination}
import fr.renoux.gaston.input.problemFromClassPath
import fr.renoux.gaston.model.Problem
import fr.renoux.gaston.util.CanAddDuration.given
import fr.renoux.gaston.util.{Chrono, Context, Tools}

import java.time.Instant
import scala.concurrent.duration.*

object PerformanceAnalysis extends App {

  private val udoConProblem = problemFromClassPath("udocon2019/uc19.conf").force
  // TODO check a solution
  // 140+ iterations, 850+ score

  // Score(930.2745614504889) after 162 iterations
  // Score(933.5346053425618) after 163 iterations
  // Score(933.5346053425618) after 163 iterations

  // with the caches
  // Score(927.674195444651) after 207 iterations

  val tools: Tools = Tools(new Chrono(blocking = true))
  given Problem = udoConProblem
  given Context = Context(tools = tools)

  val duration: FiniteDuration = 1.minutes
  val globalSeed: Long = 0L

  given Engine = new GreedyEngine
  given Output = Output.silent
  val orchestrator = new ParallelRunner(seed = globalSeed)
  val termination = Termination(timeout = Some(Instant.now() + duration))

  val (schedule, count) = tools.chrono("Total") {
    orchestrator.run(termination)
  }

  println(s"${schedule.score} after $count iterations")
  println(s"Times: ${tools.chrono.timesTotalPretty}")
  println(s"Counts: ${tools.chrono.countsPretty}")
  println(s"Average times: ${tools.chrono.timesAveragePretty}")

}
