package fr.renoux.gaston.benchmarks

import com.google.common.math.Stats
import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.command.{Output, ParallelRunner}
import fr.renoux.gaston.engine._
import fr.renoux.gaston.input._
import fr.renoux.gaston.model.{Problem, Score}
import fr.renoux.gaston.util.CanAddDuration._
import fr.renoux.gaston.util.Context

import java.time.Instant
import scala.concurrent.duration._
import scala.util.{Random, Try}

// scalastyle:off magic.number
object LongRunnerBenchmark extends App {
  /* Input is the number of hours available. We have two benchmarks to run (Udocon and R3) so half the time for each. Less than 2h isn't possible. */
  val availableMinutesPerBenchmark = 30 * math.max(Try(args.head.toInt).getOrElse(2), 2)
  println(s"Available minutes for the benchmark: $availableMinutesPerBenchmark")

  /* Trying to balance the number of runs and the duration of them */
  val (engineRunsPerBenchmark, engineRunsDuration) = {
    if (availableMinutesPerBenchmark <= 120) (availableMinutesPerBenchmark / 6, 6.minutes)
    else if (availableMinutesPerBenchmark <= 400) (20, (availableMinutesPerBenchmark / 20).minutes)
    else (availableMinutesPerBenchmark / 20, 20.minutes)
  }


  private val udocon2019: Problem = problemFromClassPath("udocon2019/uc19.conf").force
  private val r32024: Problem = problemFromClassPath("r32024/full.conf").force
  private implicit val context: Context = Context.Default
  private implicit val random: Random = new Random

  private def runEngine(problem: Problem, duration: FiniteDuration, parallelism: Int): (Score, Long) = {
    implicit val p: Problem = problem
    implicit val engine: Engine = new GreedyEngine
    implicit val output: Output = Output.silent
    val seed = random.nextLong()

    val runner = new ParallelRunner(seed = seed, parallelism = parallelism)
    val termination: Termination = Termination(timeout = Some(Instant.now() + duration))

    val (schedule, count) = runner.run(termination)
    (schedule.score, count)
  }

  private def runBenchmark(problem: Problem, name: String) = {
    val scores = new Array[Double](engineRunsPerBenchmark)
    val counts = new Array[Long](engineRunsPerBenchmark)
    var i = 0 // scalastyle:ignore var.local
    while (i < engineRunsPerBenchmark) {
      val (score, count) = runEngine(problem, engineRunsDuration, 6)
      scores(i) = score.value
      counts(i) = count
      i += 1
    }

    val scoreStats = Stats.of(scores: _*)
    val countStats = Stats.of(counts: _*)

    println(
      s"""Problem: $name
         |  Score mean: ${scoreStats.mean()}
         |    variance: ${scoreStats.sampleStandardDeviation()}
         |    min: ${scoreStats.min()}
         |    max: ${scoreStats.max()}
         |  Count mean: ${countStats.mean()}
         |    variance: ${countStats.sampleStandardDeviation()}
         |    min: ${countStats.min()}
         |    max: ${countStats.max()}
         |""".stripMargin)
  }

  println(s"Each benchmark will be $engineRunsPerBenchmark runs, ${engineRunsDuration.toMinutes} minutes each")
  runBenchmark(udocon2019, "Udocon 2019")
  runBenchmark(r32024, "R3 2024")
}

// scalastyle:on magic.number
