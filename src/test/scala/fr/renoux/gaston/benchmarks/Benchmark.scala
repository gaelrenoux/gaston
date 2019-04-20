package fr.renoux.gaston.benchmarks

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.UdoConTestModel
import fr.renoux.gaston.command.{Output, Runner}
import fr.renoux.gaston.engine._
import fr.renoux.gaston.input._
import fr.renoux.gaston.model.{Problem, ScoredSchedule}
import fr.renoux.gaston.util.{Chrono, Opt, Tools}
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class Benchmark extends FlatSpec with Matchers {

  private val log = Logger[Benchmark]

  private val udoConProblem = problemFromClassPath("udocon-2017-completed.conf").force
  private val lastYear = UdoConTestModel.Solutions.Actual
  udoConProblem.constraints.filter(!_.isRespected(lastYear)).foreach(c => log.info(s"Constraint broken $c"))

  "Systematic improver" should "give an okay score" ignore {
    benchmark(
      improver = new ExhaustiveScheduleImprover(_),
      duration = 2.minutes,
      expectsScore = 500
    )
  }

  behavior of "Fast improver"

  it should "give an good score when working a short time" ignore {
    benchmark(
      duration = 5.minutes,
      expectsScore = 600
    )
  }

  it should "give an great score when working a long time" ignore {
    benchmark(
      duration = 20.minutes,
      expectsScore = 700
    )
  }

  /** Runs the engine with some values */
  private def benchmark(
      duration: FiniteDuration,
      seed: Long = 0L,
      problem: Problem = udoConProblem,
      expectsCount: Long = 0,
      expectsScore: Double,
      parallelRunCount: Opt[Int] = Opt.Missing,
      verbose: Boolean = true,
      improver: Problem => ScheduleImprover = new GreedyScheduleImprover(_)
  ) = {
    implicit val tools: Tools = Tools(new Chrono)
    val start = System.currentTimeMillis()

    val output = new Output
    val engine = new Engine(problem, improver)

    def printer(ss: ScoredSchedule, count: Long): Unit = if (verbose) {
      val time = (System.currentTimeMillis() - start) / 1000
      println(s"After $time seconds")
      output.writeScheduleIfBetter(ss, problem)
      output.writeAttempts(count)
    }

    val runner = parallelRunCount.toOption match {
      case None => new Runner(problem, engine, hook = printer)
      case Some(prc) => new Runner(problem, engine, hook = printer, parallelRunCount = prc)
    }

    val (ScoredSchedule(schedule, score), count) = runner.run(Some(duration), seed = seed)

    println(s"$score after $count iterations")
    println(s"${tools.chrono.times} in ${tools.chrono.counts}")

    schedule.isSolution should be(true)
    score.value should be > expectsScore
    count should be > expectsCount
  }
}
