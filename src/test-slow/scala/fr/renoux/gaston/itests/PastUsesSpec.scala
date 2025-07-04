package fr.renoux.gaston.itests

import fr.renoux.gaston.TestUtils.*
import fr.renoux.gaston.command.{Output, SyncRunner}
import fr.renoux.gaston.engine.{Engine, GreedyEngine, Termination}
import fr.renoux.gaston.input.problemFromClassPath
import fr.renoux.gaston.model.{Problem, Schedule}
import fr.renoux.gaston.util.Context
import org.scalatest.PrivateMethodTester
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.Span

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.{DurationInt, FiniteDuration}

class PastUsesSpec extends AnyFlatSpec with Matchers with PrivateMethodTester with TimeLimitedTests {

  override val timeLimit: Span = 5.minute

  given Context = Context.Default
  private val udocon2017: Problem = problemFromClassPath("udocon2017/uc17.conf").force
  private val udocon2019: Problem = problemFromClassPath("udocon2019/uc19.conf").force
  private val r32019: Problem = problemFromClassPath("r32019/r32019.conf").force
  private val r32023: Problem = problemFromClassPath("r32023/r32023-tts2.conf").force
  private val r32024: Problem = problemFromClassPath("r32024/full.conf").force

  private def runConcurrently(problem: Problem, iterations: Long, count: Int, timeout: FiniteDuration = 10.minutes) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val future = Future.sequence(
      (0 until count).map { c =>
        Future(run(problem, iterations, seed = 43 + c))
      }
    )
    Await.result(future, timeout).maxBy(_._1.score.value)
  }

  private def run(problem: Problem, iterations: Long, seed: Int = 43): (Schedule, Long) = {
    given Problem = problem
    given Engine = new GreedyEngine
    given Output = Output.silent
    val runner = new SyncRunner(seed = seed)
    val termination: Termination = Termination(count = Some(iterations))
    runner.run(termination)
  }

  "udocon2017" should "not crash after examining 10 schedules" in {
    val (result, count) = run(udocon2017, 10)
    println(result.toFormattedString)
    count should be(10L)
  }

  it should "return a great result after 1000 schedules" in {
    val (result, count) = runConcurrently(udocon2017, 1000, 4)
    println(result.toFormattedString)
    count should be(1000L)
    result.score.value should be > 700.0
  }

  "udocon2019" should "not crash after examining 10 schedules" in {
    val (result, count) = run(udocon2019, 10)
    println(result.toFormattedString)
    count should be(10L)
  }

  it should "return a great result after 1000 schedules" in {
    val (result, count) = runConcurrently(udocon2019, 1000, 4)
    println(result.toFormattedString)
    count should be(1000L)
    result.score.value should be > 845.0
  }

  "r32019" should "not crash after after examining 10 schedules" in {
    val (result, count) = run(r32019, 10)
    println(result.toFormattedString)
    count should be(10L)
  }

  it should "return a great result after 100 schedules" in {
    val (result, count) = runConcurrently(r32019, 100, 4)
    println(result.toFormattedString)
    count should be(100L)
    result.score.value should be > 400.0
  }

  "r32023" should "not crash after after examining 10 schedules" in {
    val (result, count) = run(r32023, 10)
    println(result.toFormattedString)
    count should be(10L)
  }

  it should "return a great result after 100 schedules" in {
    val (result, count) = runConcurrently(r32023, 100, 4)
    println(result.toFormattedString)
    count should be(100L)
    result.score.value should be > 300.0
  }

  "r32024" should "not crash after after examining 10 schedules" in {
    val (result, count) = run(r32024, 10)
    println(result.toFormattedString)
    count should be(10L)
  }

  it should "return a great result after 100 schedules" in {
    val (result, count) = runConcurrently(r32024, 100, 4)
    println(result.toFormattedString)
    count should be(100L)
    result.score.value should be > 600.0
  }

}
