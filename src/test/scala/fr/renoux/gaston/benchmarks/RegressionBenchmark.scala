package fr.renoux.gaston.benchmarks

import java.time.Instant

import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.command.Runner
import fr.renoux.gaston.engine._
import fr.renoux.gaston.input._
import fr.renoux.gaston.model.Problem
import fr.renoux.gaston.util.{Context, Opt}
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

// scalastyle:off magic.number

class RegressionBenchmark extends FlatSpec with Matchers {
  import Ordering.Double.IeeeOrdering

  private val udoConProblem = problemFromClassPath("udocon2017/uc17-completed.conf").force

  behavior of "Engine"

  it should "give an good score when working a short time" ignore {
    benchmark(
      duration = 5.minutes,
      expectsScore = 730,
      expectsCount = 200
    )
  }

  it should "give an great score when working a long time" ignore {
    benchmark(
      duration = 20.minutes,
      expectsScore = 750,
      expectsCount = 800
    )
  }

  /** Runs the engine with some values */
  private def benchmark(
      duration: FiniteDuration,
      seed: Long = 0L,
      problem: Problem = udoConProblem,
      context: Context = Context.Default,
      expectsCount: Long,
      expectsScore: Double,
      parallelRunCount: Opt[Int] = Opt.Missing
  ): Unit = {
    val timeout = Instant.now().plusMillis(duration.toMillis)
    val engine = new Engine(stopAtScore = expectsScore, backtrackInitialSchedule = true, timeout = timeout)(problem, context)

    val handler = logMinutes(true) // (verbose)

    val _ = try {
      val runner = parallelRunCount.toOption match {
        case None => new Runner(engine)(problem, context)
        case Some(prc) => new Runner(engine, parallelRunCount = prc)(problem, context)
      }

      val (schedule, count) = runner.run(Some(duration), seed = seed)

      println(s"${schedule.score} after $count iterations")

      schedule.problem.constraints.filterNot(_.isRespected(schedule)) should be(Set.empty)
      schedule.isSolution should be(true)
      schedule.score.value should be > expectsScore
      count should be > expectsCount
    } finally handler.stop()
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

// scalastyle:on magic.number
