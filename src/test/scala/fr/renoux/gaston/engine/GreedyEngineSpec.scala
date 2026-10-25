package fr.renoux.gaston.engine

import fr.renoux.gaston.SimpleTestModel
import fr.renoux.gaston.model.{Problem, Schedule}
import fr.renoux.gaston.util.Context
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class GreedyEngineSpec extends AnyFlatSpec with Matchers {

  given Context = Context.Default

  private def results(problem: Problem, seed: Long, termination: Termination): List[(Schedule, Long)] = {
    given Problem = problem
    new GreedyEngine().lazySeq(seed, termination).toList
  }

  private def checkSequence(sequence: List[(Schedule, Long)]): Unit = {
    sequence should not be empty
    sequence.head._2 should be(1)
    sequence.foreach { case (schedule, _) => schedule.isSolution should be(true) }
    sequence.sliding(2).foreach {
      case List((s1, a1), (s2, a2)) =>
        s2.score should be > s1.score
        a2 should be > a1
      case _ =>
    }
  }

  "lazySeq" should "produce strictly improving schedules, with unassigned topics" in {
    checkSequence(results(SimpleTestModel.Problems.WithUnassignedTopics, seed = 0, Termination(count = Some(200))))
  }

  it should "produce strictly improving schedules, without unassigned topics" in {
    checkSequence(results(SimpleTestModel.Problems.NoUnassignedTopics, seed = 0, Termination(count = Some(200))))
  }

  it should "stop when reaching the max count" in {
    val sequence = results(SimpleTestModel.Problems.WithUnassignedTopics, seed = 0, Termination(count = Some(5)))
    sequence.last._2 should be <= 5L
  }

  it should "stop once the target score is reached" in {
    val target = SimpleTestModel.Solutions.BestWithUnassignedTopics.score
    val sequence = results(SimpleTestModel.Problems.WithUnassignedTopics, seed = 0, Termination(score = Some(target)))
    sequence.init.foreach { case (schedule, _) => schedule.score should be < target }
  }

  it should "be deterministic for a given seed" in {
    val termination = Termination(count = Some(50))
    results(SimpleTestModel.Problems.WithUnassignedTopics, seed = 7, termination) should be(
      results(SimpleTestModel.Problems.WithUnassignedTopics, seed = 7, termination)
    )
  }

}
