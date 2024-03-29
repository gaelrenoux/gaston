package fr.renoux.gaston.engine

import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.command.Runner
import fr.renoux.gaston.input.problemFromClassPath
import fr.renoux.gaston.model.{Problem, Schedule}
import fr.renoux.gaston.util.Context
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// scalastyle:off magic.number
class EngineSpec extends AnyFlatSpec with Matchers with PrivateMethodTester {

  private implicit val context: Context = Context.Default
  private val problem17: Problem = problemFromClassPath("udocon2017/uc17.conf").force
  private val problem19: Problem = problemFromClassPath("udocon2019/uc19.conf").force

  private def run(problem: Problem, iterations: Long): (Schedule, Long) = {
    implicit val p: Problem = problem
    implicit val i: GreedySlotImprover = new GreedySlotImprover
    implicit val engine: Engine = new Engine(backtrackInitialSchedule = true)
    val runner = new Runner(parallelRunCount = 1)
    val params: OptimParams = OptimParams(maxIterations = Some(iterations))
    runner.run(seed = 42, params)
  }

  "problem 17" should "return a good result after 10 iterations" in {
    val (result, count) =  run(problem17, 10)
    println(result.toFormattedString)
    println(count)
    count should be(10)
    result.score.value should be > 519.0
  }

  it should "return a great result after 100 iterations" in {
    val (result, count) =  run(problem17, 100)
    println(result.toFormattedString)
    println(count)
    count should be(100)
    result.score.value should be > 725.0
  }

  "problem 19" should "return a good result after 10 iterations" in {
    val (result, count) =  run(problem19, 10)
    println(result.toFormattedString)
    println(count)
    count should be(10)
    result.score.value should be > 657.0
  }

  it should "return a great result after 100 iterations" in {
    val (result, count) =  run(problem19, 100)
    println(result.toFormattedString)
    println(count)
    count should be(100)
    result.score.value should be > 970.0
  }

}
