package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.input.{PureConfigLoader, InputSettings}
import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}

import scala.util.Random


class ConstrainedScheduleFactorySpec extends FlatSpec with Matchers with PrivateMethodTester {
  private val log = Logger[ConstrainedScheduleFactorySpec]
  implicit val settings: InputSettings = PureConfigLoader.fromDefault.forceToInput.gaston.settings
  val SimpleTestModel = fr.renoux.gaston.SimpleTestModel(settings)
  val ComplexTestModel = fr.renoux.gaston.ComplexTestModel(42L)
  private val random = new Random(0L)

  assert(SimpleTestModel.Problems.Complete.isSolvedBy(SimpleTestModel.Solutions.Best))


  behavior of "makePartialSchedule"

  it should "work out a partial schedule when there is one (on a simple model)" in {
    val factory = new ConstrainedScheduleFactory(SimpleTestModel.Problems.Complete)
    val solution = factory.makePartialSchedule(random)
    log.info(s"Solution: $solution")
    solution.isDefined should be(true)
  }

  it should "work out a partial schedule when there is one (on a complex model)" in {
    val factory = new ConstrainedScheduleFactory(ComplexTestModel.Problems.Complete)
    val solution = factory.makePartialSchedule(random)
    log.info(s"Solution: $solution")
    solution.isDefined should be(true)
  }


  behavior of "makeSchedule"

  it should "work out a schedule when there is one (on a simple model)" in {
    val factory = new ConstrainedScheduleFactory(SimpleTestModel.Problems.Complete)
    val solution = factory.makeSchedule(random)
    solution foreach { s =>
      log.info(s"Solution: ${s.toFormattedString}")
    }
    solution.isDefined should be(true)
  }

  it should "work out a schedule when there is one (on a complex model)" in {
    val factory = new ConstrainedScheduleFactory(ComplexTestModel.Problems.Complete)
    val solution = factory.makeSchedule(random)
    solution foreach { s =>
      log.info(s"Solution: ${s.toFormattedString}")
    }
    solution.isDefined should be(true)
  }

  it should "always return a valid schedule if any (on a simple model)" in {
    val factory = new ConstrainedScheduleFactory(SimpleTestModel.Problems.Complete)
    val solution = factory.makeSchedule(random)
    solution foreach { s =>
      log.info(s"Solution: ${s.toFormattedString}")
      SimpleTestModel.Problems.Complete.constraints.filter(!_.isRespected(s)) should be(Set())
    }
  }

  it should "always return a valid schedule if any (on a complex model)" in {
    val factory = new ConstrainedScheduleFactory(ComplexTestModel.Problems.Complete)
    val solution = factory.makeSchedule(random)
    solution foreach { s =>
      log.info(s"Solution: ${s.toFormattedString}")
      SimpleTestModel.Problems.Complete.constraints.filter(!_.isRespected(s)) should be(Set())
    }
  }

}
