package fr.renoux.gaston.benchmarks

import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.command.{Output, ParallelRunner}
import fr.renoux.gaston.engine._
import fr.renoux.gaston.input._
import fr.renoux.gaston.model.{Problem, Score}
import fr.renoux.gaston.util.CanAddDuration._
import fr.renoux.gaston.util.{Context, Opt}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

// scalastyle:off magic.number

class RegressionBenchmark extends AnyFlatSpec with Matchers {

  import Ordering.Double.IeeeOrdering

  private implicit val udoConProblem: Problem = problemFromClassPath("udocon2017/uc17.conf").force
  private implicit val context: Context = Context.Default

  behavior of "Engine"

  it should "give an good score when working 1 min" in {
    benchmark(
      duration = 1.minute,
      expectsScore = 715,
      expectsCount = 80 * 1000
    )
  }

  it should "give a good score when working 5 min" in {
    benchmark(
      duration = 5.minutes,
      expectsScore = 760,
      expectsCount = 1000 * 1000
    )
  }

  it should "give a great score when working 20 min" in {
    benchmark(
      duration = 20.minutes,
      expectsScore = 770,
      expectsCount = 4 * 1000 * 1000
    )
  }

  /** Runs the engine with some values */
  private def benchmark(
      duration: FiniteDuration,
      globalSeed: Long = 0L,
      expectsCount: Long,
      expectsScore: Double,
      parallelRunCount: Opt[Int] = Opt.Missing
  ): Unit = {
    implicit val engine: Engine = new GreedyEngine
    implicit val output: Output = Output.silent

    val handler = logMinutes()

    val _ = try {
      val runner = parallelRunCount.toOption match {
        case None => new ParallelRunner(seed = globalSeed)
        case Some(prc) => new ParallelRunner(seed = globalSeed, parallelism = prc)
      }

      val termination: Termination = Termination(score = Some(Score(expectsScore)), timeout = Some(Instant.now() + duration))
      val (schedule, count) = runner.run(termination)

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

  private def logMinutes(): Stoppable = new Stoppable {
    private var continue = true
    var lastMinute = 0
    private val startTime = System.currentTimeMillis()

    def stop(): Unit = {
      continue = false
    }

    Future {
      while (continue) {
        Thread.sleep(1000)
        if (System.currentTimeMillis() - startTime > 60000 * lastMinute) {
          println(s"Minute $lastMinute")
          lastMinute = lastMinute + 1
        }
      }
    }(ExecutionContext.global)
  }
}

// scalastyle:on magic.number
