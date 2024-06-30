package fr.renoux.gaston.engine.assignment

import fr.renoux.gaston.model.{Problem, Record, Schedule}
import fr.renoux.gaston.util.Context
import fr.renoux.gaston.{MinimalTestModel, SimpleTestModel}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class RandomAssignerSpec extends AnyFlatSpec with Matchers {

  implicit val context: Context = Context.Default

  behavior of "minimal model"

  it should "be filled in" in {
    implicit val rand: Random = new Random(0)
    implicit val problem: Problem = MinimalTestModel.Problems.Minimal
    import fr.renoux.gaston.MinimalTestModel.Slots._
    import fr.renoux.gaston.MinimalTestModel.Topics
    import fr.renoux.gaston.MinimalTestModel.Topics._

    val randomAssigner = new RandomAssigner
    val partialSchedule = Schedule.from(
      Topics.Unassigned.map { case (slot, topic) => Record(slot, topic) }.toSeq ++
        Morning(Fighting()) ++ Afternoon(Machines()) ++ Evening(Leading()) ++ Night(Party())
    )
    val filledSchedule = randomAssigner.fill(partialSchedule)

    filledSchedule.nonEmpty should be(true)
    println(filledSchedule.get.toFormattedString)
    filledSchedule.get.isPartialSolution should be(true)
    filledSchedule.get.isSolution should be(true)
  }

  behavior of "simple model"

  it should "fill in a schedule where one person is unscheduled" in {
    implicit val rand: Random = new Random(0)
    implicit val problem: Problem = SimpleTestModel.Problems.Complete
    import fr.renoux.gaston.SimpleTestModel.Slots
    import fr.renoux.gaston.SimpleTestModel.Slots._
    import fr.renoux.gaston.SimpleTestModel.Topics._
    import fr.renoux.gaston.SimpleTestModel.Persons._

    val randomAssigner = new RandomAssigner

    val baseSchedule = SimpleTestModel.Solutions.Best.updateSlotSchedule(Morning) { ss =>
      ss.updateTopicRecord(Grinding)(_.removePerson(Fiona))
    }
    val testFilled = randomAssigner.fill(baseSchedule)
    println(testFilled.map(_.toFormattedString))
    testFilled.nonEmpty should be(true)
    testFilled.get should be(SimpleTestModel.Solutions.Best)
  }

  it should "fill in an empty schedule" in {
    implicit val rand: Random = new Random(0)
    implicit val problem: Problem = SimpleTestModel.Problems.Complete
    import fr.renoux.gaston.SimpleTestModel.Slots
    import fr.renoux.gaston.SimpleTestModel.Slots._
    import fr.renoux.gaston.SimpleTestModel.Topics._
    import fr.renoux.gaston.SimpleTestModel.Persons._

    val randomAssigner = new RandomAssigner

    val partialSchedule = SimpleTestModel.Solutions.Best.clearSlots(Slots.All.flatten: _*)
    val filledSchedule = randomAssigner.fill(partialSchedule)

    filledSchedule.nonEmpty should be(true)
    println(filledSchedule.get.toFormattedString)
    filledSchedule.get.isPartialSolution should be(true)
    filledSchedule.get.isSolution should be(true)
  }

}
