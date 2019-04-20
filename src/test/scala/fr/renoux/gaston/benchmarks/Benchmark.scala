package fr.renoux.gaston.benchmarks

import java.text.DecimalFormat

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.UdoConTestModel
import fr.renoux.gaston.command.{Output, Runner}
import fr.renoux.gaston.engine._
import fr.renoux.gaston.input._
import fr.renoux.gaston.model.{Score, ScoredSchedule}
import fr.renoux.gaston.util.{Chrono, Tools}
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._
import scala.util.Random

class Benchmark extends FlatSpec with Matchers {
  private val log = Logger[Benchmark]

  private val udoConProblem = problemFromClassPath("udocon-2017-completed.conf").force
  private val lastYear = UdoConTestModel.Solutions.Actual
  udoConProblem.constraints.filter(!_.isRespected(lastYear)).foreach(c => log.info(s"Constraint broken $c"))
  private val duration = 2.minutes


  "Systematic improver" should "give a good score" ignore {
    implicit val tools: Tools = Tools(new Chrono)

    val engine = new Engine(udoConProblem, new ExhaustiveScheduleImprover(_))
    val runner = new Runner(udoConProblem, engine)
    val (ScoredSchedule(schedule, score), count) = runner.run(Some(duration), seed = 0L)

    log.debug(s"Tested improver produced: ${schedule.toFormattedString}")

    println(s"$score after $count iterations")
    println(s"${tools.chrono.times} in ${tools.chrono.counts}")

    schedule.isSolution should be(true)
    score.value should be > 500.0
    count should be > 50L

  }


  "Fast improver" should "give an good score" ignore {
    implicit val tools: Tools = Tools(new Chrono)

    val output = new Output
    val engine = new Engine(udoConProblem, new GreedyScheduleImprover(_))
    val runner = new Runner(udoConProblem, engine, hook = (ss, count) => {
      output.writeScheduleIfBetter(ss, udoConProblem, UdoConTestModel.Settings)
      output.writeAttempts(count)
    })
    val (ScoredSchedule(schedule, score), count) = runner.run(Some(duration), seed = 0L)

    println(s"$score after $count iterations")
    println(s"${tools.chrono.times} in ${tools.chrono.counts}")

    schedule.isSolution should be(true)
    score.value should be > 500.0
    count should be > 125L
  }

  "Compare improvers on various schedules" should "work" ignore {
    implicit val tools: Tools = Tools(new Chrono)

    val seedFormat = new DecimalFormat("000")
    val scoreFormat = new DecimalFormat("0000")
    val durationFormat = new DecimalFormat("0000")

    def format(score: Score, duration: Long) =
      s"${durationFormat.format(duration)} ms   ${scoreFormat.format(score.value.round)}"


    val engine = new Engine(udoConProblem)
    val systematicImprover = new ExhaustiveScheduleImprover(udoConProblem)
    val fastImprover = new GreedyScheduleImprover(udoConProblem)

    println("       Systematic        Fast               ")
    for (seed <- 0 to 100) {
      implicit val rand: Random = new Random(seed)

      val Some(initialSolution) = engine.generateUnimproved
      val initialScore = Scorer.score(initialSolution)

      val sysStart = System.currentTimeMillis()
      val sysSolution = systematicImprover.improve(initialSolution, initialScore)
      val sysDuration = System.currentTimeMillis() - sysStart
      val sysScore = Scorer.score(sysSolution)

      val fstStart = System.currentTimeMillis()
      val fstSolution = fastImprover.improve(initialSolution, initialScore)
      val fstDuration = System.currentTimeMillis() - fstStart
      val fstScore = Scorer.score(fstSolution)

      val txt = s"${seedFormat.format(seed.toLong)}    ${format(sysScore, sysDuration)}    ${format(fstScore, fstDuration)}"
      if (fstScore.value.round == sysScore.value.round) println(txt)
      else System.err.println(txt)
    }
  }
}
