package fr.renoux.gaston.benchmarks

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.UdoConTestModel
import fr.renoux.gaston.command.{Output, Runner}
import fr.renoux.gaston.engine._
import fr.renoux.gaston.input._
import fr.renoux.gaston.model.{Problem, Schedule}
import fr.renoux.gaston.util.{Chrono, Opt, Tools}
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

class Benchmark extends FlatSpec with Matchers {

  private val log = Logger[Benchmark]

  private val udoConProblem = problemFromClassPath("udocon2017/uc17-completed.conf").force
  private val lastYear = UdoConTestModel.Solutions.Actual
  udoConProblem.constraints.filter(!_.isRespected(lastYear)).foreach(c => log.info(s"Constraint broken $c"))

  behavior of "Engine"

  it should "give an good score when working a short time" in {
    benchmark(
      duration = 5.minutes,
      expectsScore = 600
    )
  }

  it should "give an great score when working a long time" ignore {
    benchmark(
      duration = 20.minutes,
      expectsScore = 700,
      parallelRunCount = 1
    )
  }

  /** Runs the engine with some values */
  private def benchmark(
      duration: FiniteDuration,
      seed: Long = 0L,
      problem: Problem = udoConProblem,
      context: Context = Context.Default,
      expectsCount: Long = 0,
      expectsScore: Double,
      parallelRunCount: Opt[Int] = Opt.Missing,
      verbose: Boolean = true
  ): Unit = {
    val tools: Tools = Tools(new Chrono)
    val start = System.currentTimeMillis()

    val output = new Output()(problem)
    val engine = new Engine(stopAtScore = expectsScore, backtrackInitialSchedule = true)(problem, context)

    def printer(ss: Schedule, count: Long): Unit = if (verbose) {
      val time = (System.currentTimeMillis() - start) / 1000
      println(s"After $time seconds")
      output.writeScheduleIfBetter(ss)
      output.writeAttempts(count)
    }

    val handler = logMinutes(false) // (verbose)

    val runner = parallelRunCount.toOption match {
      case None => new Runner( engine, hook = printer)(problem)
      case Some(prc) => new Runner( engine, hook = printer, parallelRunCount = prc)(problem)
    }

    val (schedule, count) = runner.run(Some(duration), seed = seed)

    println(s"${schedule.score} after $count iterations")
    println(s"${tools.chrono.times} in ${tools.chrono.counts}")

    schedule.problem.constraints.filterNot(_.isRespected(schedule)) should be(Set())
    schedule.isSolution should be(true)
    schedule.score.value should be > expectsScore
    count should be > expectsCount
    handler.stop()
  }

  trait Stoppable {
    def stop(): Unit
  }

  private def logMinutes(verbose: Boolean): Stoppable = new Stoppable {
    private var continue = verbose
    var lastMinute = 0
    private val startTime = System.currentTimeMillis()

    def stop(): Unit = {
      continue = false
    }

    Future {
      while (continue) {
        Thread.sleep(1000)
        if (System.currentTimeMillis() - startTime > 60000 * lastMinute) {
          lastMinute = lastMinute + 1
          println(s"Minute $lastMinute")
        }
      }
    }(ExecutionContext.global)
  }
}
