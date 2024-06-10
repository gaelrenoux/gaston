package fr.renoux.gaston.itests

import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.command.{Output, Runner}
import fr.renoux.gaston.engine.{Engine, GreedySlotImprover, OptimParams}
import fr.renoux.gaston.input.problemFromClassPath
import fr.renoux.gaston.model.{Problem, Schedule}
import fr.renoux.gaston.util.Context
import org.scalatest.PrivateMethodTester
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.Span

import scala.concurrent.duration.DurationInt

// scalastyle:off magic.number
class PastUsesSpec extends AnyFlatSpec with Matchers with PrivateMethodTester with TimeLimitedTests {

  override val timeLimit: Span = 5.minute

  private implicit val context: Context = Context.Default
  private val udocon2017: Problem = problemFromClassPath("udocon2017/uc17.conf").force
  private val udocon2019: Problem = problemFromClassPath("udocon2019/uc19.conf").force
  private val r32019: Problem = problemFromClassPath("r32019/r32019.conf").force

  private def run(problem: Problem, iterations: Long): (Schedule, Long) = {
    implicit val p: Problem = problem
    implicit val i: GreedySlotImprover = new GreedySlotImprover
    implicit val engine: Engine = new Engine
    implicit val output: Output = Output.silent
    val runner = new Runner(parallelism = 1)
    val params: OptimParams = OptimParams(maxIterations = Some(iterations))
    runner.run(globalSeed = 42, params)
  }

  "udocon2017" should "return a good result after 10 iterations" in {
    val (result, count) = run(udocon2017, 10)
    println(result.toFormattedString)
    println(count)
    count should be > 10L
    result.score.value should be > 519.0
  }

  it should "return a great result after 100 iterations" in {
    val (result, count) = run(udocon2017, 100)
    println(result.toFormattedString)
    println(count)
    count should be> 100L
    result.score.value should be > 729.0
  }

  "udocon2019" should "return a good result after 10 iterations" in {
    val (result, count) = run(udocon2019, 10)
    println(result.toFormattedString)
    println(count)
    count should be > 10L
    result.score.value should be > 657.0
  }

  it should "return a great result after 100 iterations" in {
    val (result, count) = run(udocon2019, 100)
    println(result.toFormattedString)
    println(count)
    count should be > 100L
    result.score.value should be > 970.0
  }

  "r32019" should "return a good result after 10 iterations" in {
    val (result, count) = run(r32019, 10)
    println(result.toFormattedString)
    println(count)
    count should be > 10L
    result.score.value should be > 657.0
  }

  it should "return a great result after 100 iterations" in {
    val (result, count) = run(r32019, 100)
    println(result.toFormattedString)
    println(count)
    count should be > 100L
    result.score.value should be > 970.0
  }

}
