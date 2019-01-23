package fr.renoux.gaston.benchmarks

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.UdoConTestModel
import fr.renoux.gaston.engine.{GreedyScheduleImprover, Runner, SystematicScheduleImprover}
import fr.renoux.gaston.input.InputLoader
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class Benchmark extends FlatSpec with Matchers {
  private val log = Logger[Benchmark]

  private val udoConProblem = InputLoader.fromClassPath("udocon-2017-completed.conf").forceToModel
  private val lastYear = UdoConTestModel.Solutions.Actual
  udoConProblem.constraints.filter(!_.isRespected(lastYear)).foreach(c => log.info(s"Constraint broken $c"))
  private val duration = 2.minutes


  "Systematic improver" should "give a good score" ignore {

    val runner = new Runner(udoConProblem, improverConstructor = new SystematicScheduleImprover(_))
    val (schedule, score, count) = runner.run(Some(duration), seed = 0L)

    log.debug(s"Tested improver produced: ${schedule.toFormattedString}")

    println(s"$score after $count iterations")
    udoConProblem.isSolvedBy(schedule) should be(true)
    score.value should be > 500.0
    count should be > 50L
  }


  "Greedy improver" should "give an okay score" ignore {

    val runner = new Runner(udoConProblem, improverConstructor = new GreedyScheduleImprover(_))
    val (schedule, score, count) = runner.run(Some(duration), seed = 0L)

    println(s"$score after $count iterations")
    udoConProblem.isSolvedBy(schedule) should be(true)
    score.value should be > 500.0
    count should be > 100L
  }
}
