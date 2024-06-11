package fr.renoux.gaston.engine

import fr.renoux.gaston.SimpleTestModel
import fr.renoux.gaston.command.{Output, SyncRunner}
import fr.renoux.gaston.model.{Problem, Schedule}
import fr.renoux.gaston.util.Context
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// scalastyle:off magic.number
class SyncRunnerSpec extends AnyFlatSpec with Matchers with PrivateMethodTester {

  private implicit val context: Context = Context.Default

  private def run(problem: Problem, iterations: Long, seed: Long): (Schedule, Long) = {
    implicit val p: Problem = problem
    implicit val i: GreedySlotImprover = new GreedySlotImprover
    implicit val engine: Engine = new Engine
    implicit val output: Output = Output.silent
    val runner = new SyncRunner(seed)
    val termination: Termination = Termination(count = Some(iterations))
    runner.run(termination)
  }

  "Given a fixed seed, the runner" should "always return the same result after 10 iterations" in {
    val results =
      for {_ <- 0 until 10} yield {
        run(SimpleTestModel.Problems.Complete, iterations = 10, seed = 42)
      }

    results.map(_._1).toSet.size should be(1) // always the same schedule
    results.map(_._1.score).toSet.size should be(1) // always with the same score
    results.map(_._1.chainSeed).toSet.size should be(1) // always with the same seed
    results.map(_._2).toSet.size should be(1) // always seeing the same number of schedules

    println(results.head._1.toFormattedString)
    // TODO Check out why it still has a very incomplete schedule in 10 iterations

    val otherResult = run(SimpleTestModel.Problems.Complete, iterations = 10, seed = 43)
    otherResult._2 should be(10)
    otherResult._1 shouldNot be(results.head._1)
    otherResult._1.score shouldNot be(results.head._1.score)
    otherResult._1.chainSeed shouldNot be(results.head._1.chainSeed)
    // not enough iterations to compare the number of schedules seen, it'll often be 10 anyway

    println(otherResult._1.toFormattedString)
  }

  "Given a fixed seed, the runner" should "always return the same result after 100 iterations" in {
    val results =
      for {_ <- 0 until 3} yield {
        run(SimpleTestModel.Problems.Complete, iterations = 100, seed = 666)
      }

    // we'll always have reached the optimal solution on this problem in 100 iterations, so we must compare the seeds
    results.map(_._1).toSet.size should be(1) // always the same schedule
    results.map(_._1.score).toSet.size should be(1) // always with the same score
    results.map(_._1.chainSeed).toSet.size should be(1) // always with the same seed
    results.map(_._2).toSet.size should be(1) // always seeing the same number of schedules

    println(results.head._2)
    println(results.head._1.toFormattedString)

    val otherResult = run(SimpleTestModel.Problems.Complete, iterations = 100, seed = 777)
    // since we'll have the optimial solution, we must compare the seeds only. Number of seen schedules should also be different.
    otherResult._1.chainSeed shouldNot be(results.head._1.chainSeed)
    otherResult._2 shouldNot be(results.head._2)

    println(otherResult._2)
    println(otherResult._1.toFormattedString)
  }

}
