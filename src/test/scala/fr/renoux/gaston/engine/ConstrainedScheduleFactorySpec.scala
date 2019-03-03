package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.{SimpleTestModel, UdoConTestModel}
import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}

import scala.util.Random


class ConstrainedScheduleFactorySpec extends FlatSpec with Matchers with PrivateMethodTester {
  private val log = Logger[ConstrainedScheduleFactorySpec]
  val ComplexTestModel = fr.renoux.gaston.ComplexTestModel(42L)
  private val random = new Random(0L)

  assert(SimpleTestModel.Solutions.Best.isSolution)


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


  behavior of "upperLimit"

  import ConstrainedScheduleFactory.upperLimit

  it should "work on a very small number" in {
    upperLimit(slots = 2, topics = 3, minTopicsPerSlot = 1, maxTopicsPerSlot = 2) should be(12)
    //A-BC, B-AC, C-AB, AB-C, AC-B, BC-A, A-B, B-A, A-C, C-A, B-C, C-B
  }

  it should "work on a small number" in {
    upperLimit(slots = 2, topics = 4, minTopicsPerSlot = 1, maxTopicsPerSlot = 3) should be(50)
  }

  it should "work on a medium number" in {
    upperLimit(slots = 5, topics = 20, minTopicsPerSlot = 3, maxTopicsPerSlot = 4) should be(82353278007000L)
  }

  it should "work on a big number" in {
    val factory = new ConstrainedScheduleFactory(ComplexTestModel.Problems.Complete)
    factory.upperLimit should be(1867557041179830000L)
  }

  it should "work on a bigger number" in {
    val factory = new ConstrainedScheduleFactory(UdoConTestModel.Problems.Complete)
    factory.upperLimit should be(BigInt("346796529353273237958720"))

  }

}
