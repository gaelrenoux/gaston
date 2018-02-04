package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.io.{InputLoader, InputSettings}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
  * Created by gael on 07/05/17.
  */
class ConstrainedScheduleFactorySpec extends FlatSpec with Matchers {
  val log = Logger[ConstrainedScheduleFactorySpec]
  implicit val settings: InputSettings = InputLoader.fromClassPath.forceToInput.gaston.settings
  val SimpleTestModel = fr.renoux.gaston.SimpleTestModel(settings)

  import fr.renoux.gaston._

  "makeSchedule" should "work out a schedule when there is one (on a simple model)" in {
    val factory = new ConstrainedScheduleFactory(SimpleTestModel.Problems.Complete)
    val solution = factory.makeSchedule(new Random(0L))
    log.debug(s"Solution: $solution")
    solution.isDefined should be(true)
  }

  it should "work out a schedule when there is one (on a complex model)" in {
    val factory = new ConstrainedScheduleFactory(ComplexTestModel(42L).Problems.Complete)
    val solution = factory.makeSchedule(new Random(0L))
    log.debug(s"Solution: $solution")
    solution.isDefined should be(true)
  }

  it should "always return a valid schedule if any (on a simple model)" in {
    val factory = new ConstrainedScheduleFactory(SimpleTestModel.Problems.Complete)
    val solution = factory.makeSchedule(new Random(0L))
    solution foreach { s =>
      log.debug(s"Solution: ${s.toFormattedString}")
      SimpleTestModel.Problems.Complete.constraints.filter(!_.isRespected(s)) should be (Set())
    }
  }

  it should "always return a valid schedule if any (on a complex model)" in {
    val factory = new ConstrainedScheduleFactory(ComplexTestModel(42L).Problems.Complete)
    val solution = factory.makeSchedule(new Random(0L))
    solution foreach { s =>
      log.debug(s"Solution: ${s.toFormattedString}")
      SimpleTestModel.Problems.Complete.constraints.filter(!_.isRespected(s)) should be (Set())
    }
  }

  it should "return a schedule where topics are equitably dispatched on slots (on a simple model)" in {
    val factory = new ConstrainedScheduleFactory(SimpleTestModel.Problems.Complete)
    val solution = factory.makeSchedule(new Random(0L))
    solution foreach { s =>
      log.debug(s"Solution: ${s.toFormattedString}")
      val topicPerSlotsCounts = s.topicsPerSlot.map(_._2.size)
      topicPerSlotsCounts.max should be <= (topicPerSlotsCounts.min + 1)
    }
  }

  it should "return a schedule where topics are equitably dispatched on slots (on a complex model)" in {
    val factory = new ConstrainedScheduleFactory(ComplexTestModel(42L).Problems.Complete)
    val solution = factory.makeSchedule(new Random(0L))
    solution foreach { s =>
      log.debug(s"Solution: ${s.toFormattedString}")
      val topicPerSlotsCounts = s.topicsPerSlot.map(_._2.size)
      topicPerSlotsCounts.max should be <= (topicPerSlotsCounts.min + 1)
    }
  }

}
