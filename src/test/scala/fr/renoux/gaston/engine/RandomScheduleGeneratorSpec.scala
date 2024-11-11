package fr.renoux.gaston.engine

import fr.renoux.gaston.SimpleTestModel
import fr.renoux.gaston.TestUtils.*
import fr.renoux.gaston.input.problemFromClassPath
import fr.renoux.gaston.model.Problem
import fr.renoux.gaston.util.Context
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class RandomScheduleGeneratorSpec extends AnyFlatSpec with Matchers {

  "Generation on udocon 2017" should "work" in {
    given Problem = problemFromClassPath("udocon2017/uc17-from-table.conf").force
    given Context = Context.Debug
    given Random = new Random(0)
    val generator = new RandomScheduleGenerator(println(_))
    val schedule = generator.create(1)
    println(schedule.toFormattedString)
    schedule.isSolution should be(true)
  }

  "Generation on the simple test model, without unassigned topics" should "work" in {
    given Problem = SimpleTestModel.Problems.NoUnassignedTopics
    given Context = Context.Debug
    given Random = new Random(0)
    val generator = new RandomScheduleGenerator(println(_))
    val schedule = generator.create(1)
    println(schedule.toFormattedString)
    schedule.isSolution should be(true)
  }

  "Generation on the simple test model, with unassigned topics" should "work" in {
    given Problem = SimpleTestModel.Problems.WithUnassignedTopics
    given Context = Context.Debug
    given Random = new Random(0)
    val generator = new RandomScheduleGenerator(println(_))
    val schedule = generator.create(1)
    println(schedule.toFormattedString)
    schedule.isSolution should be(true)
  }
}
