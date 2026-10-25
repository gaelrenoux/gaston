package fr.renoux.gaston.engine

import fr.renoux.gaston.SimpleTestModel
import fr.renoux.gaston.TestUtils.*
import fr.renoux.gaston.engine.assignment.{AssignmentImprover, RandomAssigner}
import fr.renoux.gaston.input.problemFromClassPath
import fr.renoux.gaston.model.{Problem, Schedule}
import fr.renoux.gaston.util.Context
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class AssignmentImproverSpec extends AnyFlatSpec with Matchers {

  given Context = Context.Default

  /** Clears the assignment on the planning of that schedule, then fills it randomly. */
  private def randomlyReassigned(schedule: Schedule)(using Problem, Random): Schedule = {
    val unfilled = schedule.clearSlots(summon[Problem].slotsList *)
    new RandomAssigner().fill(unfilled).getOrElse(fail("Could not fill the schedule"))
  }

  private def randomAndBestAssignmentOnBestSchedule(seed: Long)(using Problem): (Schedule, Schedule) = {
    given Random = new Random(seed)
    val initial = randomlyReassigned(SimpleTestModel.Solutions.BestWithUnassignedTopics)
    val improved = new AssignmentImprover().improve(initial)
    (initial, improved)
  }

  "Improver" should "improve a simple problem without breaking anything" in {
    given Problem = SimpleTestModel.Problems.WithUnassignedTopics
    (0L until 10L).foreach { seed =>
      val (initial, improved) = randomAndBestAssignmentOnBestSchedule(seed)
      improved.isSolution should be(true)
      improved.planning should be(initial.planning)
      improved.score should be >= initial.score
    }
  }

  it should "reach a local optimum on a simple problem" in {
    given Problem = SimpleTestModel.Problems.WithUnassignedTopics
    given Random = new Random(0)
    val (_, improved) = randomAndBestAssignmentOnBestSchedule(0)
    new AssignmentImprover().improve(improved).score should be(improved.score) // no further improvement
  }

  it should "find the best assignment on the best planning of the simple problem" in {
    given Problem = SimpleTestModel.Problems.WithUnassignedTopics
    val (_, improved) = randomAndBestAssignmentOnBestSchedule(0)
    improved.score should be(SimpleTestModel.Solutions.BestWithUnassignedTopics.score)
  }

  it should "not do anything if there are no rounds left" in {
    given Problem = SimpleTestModel.Problems.WithUnassignedTopics
    given Random = new Random(0)
    val initial = randomlyReassigned(SimpleTestModel.Solutions.BestWithUnassignedTopics)
    new AssignmentImprover().improve(initial, maxRounds = 0) should be(initial)
  }

  it should "improve a complex problem without breaking anything" in {
    given problem: Problem = problemFromClassPath("udocon2017/uc17-from-table.conf").force
    given Random = new Random(0)
    val initial = randomlyReassigned(new RandomScheduleGenerator(_ => ()).create(0))
    val improved = new AssignmentImprover().improve(initial)
    improved.isSolution should be(true)
    improved.planning should be(initial.planning)
    improved.score should be > initial.score
  }

}
