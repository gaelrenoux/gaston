package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.SimpleTestModel
import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}

import scala.util.Random


class ScheduleGeneratorSpec extends FlatSpec with Matchers with PrivateMethodTester {
  private val log = Logger[ScheduleGeneratorSpec]
  val ComplexTestModel = fr.renoux.gaston.ComplexTestModel(42L)
  private implicit val random = new Random(0L)

  assert(SimpleTestModel.Solutions.Best.isSolution)


  behavior of "makeSchedule"

  it should "work out a schedule when there is one (on a simple model)" in {
    val factory = new ScheduleGenerator(SimpleTestModel.Problems.Complete)
    val solution = factory.generate
    solution foreach { s =>
      log.info(s"Solution: ${s.toFormattedString}")
    }
    solution.isDefined should be(true)
  }

  it should "work out a schedule when there is one (on a complex model)" in {
    val factory = new ScheduleGenerator(ComplexTestModel.Problems.Complete)
    val solution = factory.generate
    solution foreach { s =>
      log.info(s"Solution: ${s.toFormattedString}")
    }
    solution.isDefined should be(true)
  }

  it should "always return a valid schedule if any (on a simple model)" in {
    val factory = new ScheduleGenerator(SimpleTestModel.Problems.Complete)
    val solution = factory.generate
    solution foreach { s =>
      log.info(s"Solution: ${s.toFormattedString}")
      SimpleTestModel.Problems.Complete.constraints.filter(!_.isRespected(s)) should be(Set())
    }
  }

  it should "always return a valid schedule if any (on a complex model)" in {
    val factory = new ScheduleGenerator(ComplexTestModel.Problems.Complete)
    val solution = factory.generate
    solution foreach { s =>
      log.info(s"Solution: ${s.toFormattedString}")
      SimpleTestModel.Problems.Complete.constraints.filter(!_.isRespected(s)) should be(Set())
    }
  }

}
