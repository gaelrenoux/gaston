package fr.renoux.gaston.engine2

import fr.renoux.gaston.input.InputModel
import fr.renoux.gaston.input.InputLoader
import fr.renoux.gaston.input.InputTranscription2
import fr.renoux.gaston.TestBase
import fr.renoux.gaston.TestUtils.force
import fr.renoux.gaston.model2.ScheduleMaker.mkSchedule
import fr.renoux.gaston.model2.*
import fr.renoux.gaston.util.Context
import scala.util.Random


class AssignmentImproverTest extends TestBase {
  given Context = Context.Default

  "Nominal case" - {
    val input: InputModel = InputLoader.fromClassPath("scoring-test.conf").force
    val problem = InputTranscription2(input).result.toEither.force
    import problem.given
    given SchedulePrinter = new SchedulePrinter(problem)

    val Seq(d1, d2) = problem.slotsCount.range
    val Seq(unassigned0, unassigned1, alpha, beta, gamma, delta, epsilon1, epsilon2, eta1, eta2, theta) =
      problem.topicsCount.range
    val Seq(a, b, c, d, e, f, g, h, i, j, k, l) = problem.personsCount.range

    def scheduleBase() = mkSchedule(problem) {
      d1 slot {
        unassigned0.topicEmpty
        alpha topic (a, d, e) // Alpha, ADE
        epsilon1 topic (b, c, f) // Epsilon #1, BCF
        gamma topic (g, h, i) // Gamma, GHI
        eta1 topic (j, k, l) // Eta ~1, JKL
      }
      d2 slot {
        unassigned1.topicEmpty
        beta topic (b, c, f) // Beta, BCF
        epsilon2 topic (a, d, e) // Epsilon #2, ADE,
        delta topic (g, h, i) // Delta, GHI
        eta2 topic (j, k, l) // Eta ~2, JKL
      }
    }

    val baseScore: Score = {
      val sb = scheduleBase()
      println(sb)
      val s = sb.getTotalScore()
      println(s"Score: $s")
      s
    }

    val improver = new AssignmentImprover(problem)

    "Improved schedule should be better than the base one" in {
      given Random = new Random(0)
      val schedule = scheduleBase()
      improver.improve(schedule)
      val newScore = schedule.getTotalScore()
      newScore should be > baseScore
      println(schedule.toPrettyString)
      println(s"Score: $newScore")
    }

    "Repeated improvements should do nothing (usually)" in {
      given Random = new Random(0)
      val schedule = scheduleBase()
      improver.improve(schedule)
      val newScore = schedule.getTotalScore()
      newScore should be > baseScore
      //println(schedule.toPrettyString)
      //println(s"Score: $newScore")
      improver.improve(schedule)
      val newNewScore = schedule.getTotalScore()
      newNewScore should be(newScore)
      //println(schedule.toPrettyString)
      //println(s"Score: $newScore")
    }

    "Multiple runs should produce about the same result (usually)" in {
      given Random = new Random(0)
      val schedule1 = scheduleBase()
      improver.improve(schedule1)
      val schedule2 = scheduleBase()
      improver.improve(schedule2)
      val score1 = schedule1.getTotalScore()
      val score2 = schedule2.getTotalScore()
      score1 should be(score2)
      //println(schedule1.toPrettyString)
      //println(schedule2.toPrettyString)
    }

  }

}
