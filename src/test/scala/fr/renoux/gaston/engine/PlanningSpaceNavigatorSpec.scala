package fr.renoux.gaston.engine

import fr.renoux.gaston.SimpleTestModel
import fr.renoux.gaston.engine.PlanningSpaceNavigator.Move
import fr.renoux.gaston.model.{Problem, Schedule}
import fr.renoux.gaston.util.Context
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class PlanningSpaceNavigatorSpec extends AnyFlatSpec with Matchers {

  import SimpleTestModel.Slots.*
  import SimpleTestModel.Topics.*

  given Context = Context.Default

  given Problem = SimpleTestModel.Problems.WithUnassignedTopics

  private val navigator = new PlanningSpaceNavigator

  private def checkNeighbours(schedule: Schedule, seed: Long): List[(Schedule, Move)] = {
    given Random = new Random(seed)
    val neighbours = navigator.neighbours(schedule).toList
    neighbours.foreach { case (neighbour, move) =>
      withClue(s"$move\n${neighbour.toFormattedString}") {
        neighbour.isUnfilledSolution should be(true)
        neighbour.planning should not be schedule.planning
        move match {
          case Move.Add(slot, topics) =>
            neighbour.on(slot).topicsSet should contain allElementsOf topics
            schedule.scheduledTopics.intersect(topics) should be(Set.empty)
          case Move.Remove(slot, topics) =>
            schedule.on(slot).topicsSet should contain allElementsOf topics
            neighbour.scheduledTopics.intersect(topics) should be(Set.empty)
          case _ =>
        }
      }
    }
    neighbours
  }

  "neighbours" should "only add topics on a schedule where nothing is planned" in {
    given Random = new Random(0)
    val starting = Schedule.startingUnassignedOrForced(0)
    val neighbours = checkNeighbours(starting, seed = 0)
    neighbours should not be empty
    neighbours.foreach { case (_, move) => move shouldBe a[Move.Add] }
  }

  it should "add simultaneous topics together" in {
    given Random = new Random(0)
    val starting = Schedule.startingUnassignedOrForced(0)
    val neighbours = checkNeighbours(starting, seed = 0)
    neighbours.foreach { case (neighbour, _) =>
      neighbour.scheduledTopics.contains(Bathing) should be(neighbour.scheduledTopics.contains(Eating))
      neighbour.topicToSlot.get(Bathing) should be(neighbour.topicToSlot.get(Eating))
    }
    neighbours.map(_._2) should contain(Move.Add(AfterNoon, Set(Bathing, Eating)))
    neighbours.map(_._2) should not contain Move.Add(Morning, Set(Bathing, Eating)) // Eric is mandatory on Eating, and absent in the morning
  }

  it should "produce valid swaps and removals on a full schedule" in {
    val neighbours = checkNeighbours(SimpleTestModel.Solutions.BestWithUnassignedTopics, seed = 0)
    neighbours.exists(_._2.isInstanceOf[Move.Swap]) should be(true)
    neighbours.exists(_._2.isInstanceOf[Move.Remove]) should be(true)
  }

  it should "be deterministic for a given seed" in {
    val best = SimpleTestModel.Solutions.BestWithUnassignedTopics
    navigator.neighbours(best)(using new Random(42)).map(_._2).toList should be(
      navigator.neighbours(best)(using new Random(42)).map(_._2).toList
    )
  }

  "Move.reverts" should "recognize the reverse moves" in {
    Move.Add(Morning, Set(Acting)).reverts(Move.Remove(Morning, Set(Acting))) should be(true)
    Move.Remove(Morning, Set(Acting)).reverts(Move.Add(Morning, Set(Acting))) should be(true)
    Move.Swap(Set(Acting), Set(Bathing), isExt = false).reverts(Move.Swap(Set(Bathing), Set(Acting), isExt = false)) should be(true)
    Move.Swap(Set(Acting), Set(Bathing), isExt = true).reverts(Move.Swap(Set(Acting), Set(Bathing), isExt = true)) should be(true)
  }

  it should "not recognize other moves" in {
    Move.Add(Morning, Set(Acting)).reverts(Move.Remove(Evening, Set(Acting))) should be(false)
    Move.Add(Morning, Set(Acting)).reverts(Move.Add(Morning, Set(Acting))) should be(false)
    Move.Swap(Set(Acting), Set(Bathing), isExt = false).reverts(Move.Swap(Set(Bathing), Set(Acting), isExt = true)) should be(false)
    Move.Nothing.reverts(Move.Nothing) should be(false)
  }

}
