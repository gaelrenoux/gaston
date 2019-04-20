package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.SimpleTestModel
import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}

import scala.util.Random


class EngineSpec extends FlatSpec with Matchers with PrivateMethodTester {
  private val log = Logger[EngineSpec]
  val ComplexTestModel = fr.renoux.gaston.ComplexTestModel(42L)
  private implicit val _r: Random = new Random(0L)

  assert(SimpleTestModel.Solutions.Best.isSolution)

  behavior of "generateUnimproved"

  it should "work out a schedule when there is one (on a simple model)" in {
    val engine = new Engine(SimpleTestModel.Problems.Complete)
    val solution = engine.generateUnimproved
    solution foreach { s =>
      log.info(s"Solution: ${s.toFormattedString}")
    }
    solution.isDefined should be(true)
  }

  it should "work out a schedule when there is one (on a complex model)" in {
    val engine = new Engine(ComplexTestModel.Problems.Complete)
    val solution = engine.generateUnimproved
    solution foreach { s =>
      log.info(s"Solution: ${s.toFormattedString}")
    }
    solution.isDefined should be(true)
  }

  it should "always return a valid schedule if any (on a simple model)" in {
    val engine = new Engine(SimpleTestModel.Problems.Complete)
    val solution = engine.generateUnimproved
    solution foreach { s =>
      log.info(s"Solution: ${s.toFormattedString}")
      SimpleTestModel.Problems.Complete.constraints.filter(!_.isRespected(s)) should be(Set())
    }
  }

  it should "always return a valid schedule if any (on a complex model)" in {
    val engine = new Engine(ComplexTestModel.Problems.Complete)
    val solution = engine.generateUnimproved
    solution foreach { s =>
      log.info(s"Solution: ${s.toFormattedString}")
      SimpleTestModel.Problems.Complete.constraints.filter(!_.isRespected(s)) should be(Set())
    }
  }

}
