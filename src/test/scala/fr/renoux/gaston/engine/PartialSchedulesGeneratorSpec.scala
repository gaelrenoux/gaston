package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.SimpleTestModel
import fr.renoux.gaston.model.Problem
import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}

import scala.util.Random

class PartialSchedulesGeneratorSpec extends FlatSpec with Matchers with PrivateMethodTester {

  private val log = Logger[PartialSchedulesGeneratorSpec]

  val ComplexTestModel = fr.renoux.gaston.ComplexTestModel(42L)
  private implicit val _r: Random = new Random(0L)

  assert(SimpleTestModel.Solutions.Best.isSolution)

  behavior of "makeSchedule"

  it should "produce schedules on a simple model" in {
    test(SimpleTestModel.Problems.Complete)
  }

  it should "produce schedules on a complex model" in {
    test(ComplexTestModel.Problems.Complete)
  }

  private def test(problem: Problem): Unit = {
    val generator = new PartialSchedulesGenerator(problem)

    val solutions = generator.lazySeq
    solutions.take(3).foreach { s =>
      log.info(s"Solution: ${s.toFormattedString}")
    }
    solutions.nonEmpty should be(true)
    solutions.head.isPartialSolution should be(true)
    solutions.take(100).foreach { ps =>
      ps.isPartialSolution should be(true)
      problem.constraints.filter(c => c.isApplicableToPartialSchedule && !c.isRespected(ps)) should be(Set())
      /*problem.personsCountPerSlot.foreach { case (slot, count) =>
        ps.minNumberPerSlot.getOrElse(slot, 0) should be <= count
        ps.maxNumberPerSlot.getOrElse(slot, 0) should be >= count
      }*/
    }
  }

}
