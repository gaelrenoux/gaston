package fr.renoux.gaston.benchmarks

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.UdoConTestModel
import fr.renoux.gaston.engine.{GreedyScheduleImprover, Runner, SystematicScheduleImprover}
import fr.renoux.gaston.input.PureConfigLoader
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class Benchmark extends FlatSpec with Matchers {
  private val log = Logger[Benchmark]

  private val udoConProblem = PureConfigLoader.fromClassPath("udocon-2017-completed.conf").forceToModel
  private val lastYear = UdoConTestModel.Solutions.Actual
  udoConProblem.constraints.filter(!_.isRespected(lastYear)).foreach(c => log.info(s"Constraint broken $c"))


  "Systematic improver" should "give a good score" in {

    val runner = new Runner(udoConProblem, improverConstructor = new SystematicScheduleImprover(_))
    val (schedule, score, count) = runner.run(Some(1.minute), seed = 0L)

    println(schedule.toFormattedString)

    udoConProblem.isSolvedBy(schedule) should be(true)
    score.value should be > 2900.0
    count should be > 20L
  }


  "Greedy improver" should "give an okay score" in {

    val runner = new Runner(udoConProblem, improverConstructor = new GreedyScheduleImprover(_))
    val (schedule, score, count) = runner.run(Some(1.minute), seed = 0L)

    udoConProblem.isSolvedBy(schedule) should be(true)
    println(s"Score is $score")
    score.value should be > 2900.0
    count should be > 20L
  }
}
