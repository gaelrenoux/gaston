package fr.renoux.gaston.engine2

import fr.renoux.gaston.TestBase
import fr.renoux.gaston.TestUtils.force
import fr.renoux.gaston.input.{InputLoader, InputModel, InputTranscription2}
import fr.renoux.gaston.model2.*
import fr.renoux.gaston.model2.ScheduleMaker.mkSchedule
import fr.renoux.gaston.util.Context

import org.scalatest.Assertion

import scala.util.Random


class AssignmentImproverTest extends TestBase {
  given Context = Context.Default

  /** Checks that the schedule's indexes are coherent, and that its incrementally-maintained score is the same as the
    * one from a full recalculation. */
  private def checkConsistency(schedule: Schedule): Assertion = {
    schedule.slowCheckup should be(Nil)
    ScheduleInspector(schedule).checkAll()
    val score = schedule.getTotalScore
    schedule.recalculateAll()
    schedule.getTotalScore should be(score)
  }

  "Nominal case" - {
    val input: InputModel = InputLoader.fromClassPath("scoring-test.conf").force
    val problem = InputTranscription2(input).result.toEither.force
    given SchedulePrinter = new SchedulePrinter(problem)

    val Seq(d1, d2) = problem.slotsCount.range
    val Seq(unassigned0, unassigned1, alpha, beta, gamma, delta, epsilon1, epsilon2, eta1, eta2, theta) =
      problem.topicsCount.range
    val Seq(a, b, c, d, e, f, g, h, i, j, k, l) = problem.personsCount.range

    def scheduleBase() = mkSchedule(problem) {
      d1.slot {
        unassigned0.topicEmpty
        alpha.topic(a, d, e) // Alpha, ADE
        epsilon1.topic(b, c, f) // Epsilon #1, BCF
        gamma.topic(g, h, i) // Gamma, GHI
        eta1.topic(j, k, l) // Eta ~1, JKL
      }
      d2.slot {
        unassigned1.topicEmpty
        beta.topic(b, c, f) // Beta, BCF
        epsilon2.topic(a, d, e) // Epsilon #2, ADE,
        delta.topic(g, h, i) // Delta, GHI
        eta2.topic(j, k, l) // Eta ~2, JKL
      }
    }

    val baseScore: Score = {
      val sb = scheduleBase()
      println(sb)
      val s = sb.getTotalScore
      println(s"Score: $s")
      s
    }

    val improver = new AssignmentImprover(problem)

    "Improved schedule should be better than the base one" in {
      given Random = new Random(0)
      val schedule = scheduleBase()
      improver.improve(schedule)
      val newScore = schedule.getTotalScore
      newScore should be > baseScore
      println(schedule.toPrettyString)
      println(s"Score: $newScore")
    }

    "Repeated improvements should do nothing (usually)" in {
      given Random = new Random(0)
      val schedule = scheduleBase()
      improver.improve(schedule)
      val newScore = schedule.getTotalScore
      newScore should be > baseScore
      //println(schedule.toPrettyString)
      //println(s"Score: $newScore")
      improver.improve(schedule)
      val newNewScore = schedule.getTotalScore
      newNewScore should be(newScore)
      //println(schedule.toPrettyString)
      //println(s"Score: $newScore")
    }

    "Improved schedule should be consistent" in {
      given Random = new Random(0)
      val schedule = scheduleBase()
      improver.improve(schedule)
      checkConsistency(schedule)
    }

    "Multiple runs should produce the same result" in {
      (0 until 10000).foreach { i =>
        given Random = new Random(0)
        val schedule = scheduleBase()
        improver.improve(schedule)
        (i, schedule.getTotalScore) should be(i -> 13.8671875)
      }
    }

  }

  "Starting from an unassigned schedule" - {
    val input: InputModel = InputLoader.fromClassPath("scoring-test.conf").force
    val problem = InputTranscription2(input).result.toEither.force
    given SchedulePrinter = new SchedulePrinter(problem)

    val Seq(d1, d2) = problem.slotsCount.range
    val Seq(unassigned0, unassigned1, alpha, beta, gamma, delta, epsilon1, epsilon2, eta1, eta2, theta) =
      problem.topicsCount.range
    val Seq(a, b, c, d, e, f, g, h, i, j, k, l) = problem.personsCount.range

    def scheduleBase() = mkSchedule(problem) {
      d1.slot {
        unassigned0.topic(a, b, c, d, e, f, g, h, i, j, k, l)
        alpha.topicEmpty
        epsilon1.topicEmpty
        gamma.topicEmpty
        eta1.topicEmpty
      }
      d2.slot {
        unassigned1.topic(a, b, c, d, e, f, g, h, i, j, k, l)
        beta.topicEmpty
        epsilon2.topicEmpty
        delta.topicEmpty
        eta2.topicEmpty
      }
    }

    val baseScore: Score = {
      val sb = scheduleBase()
      println(sb)
      val s = sb.getTotalScore
      println(s"Score: $s")
      s
    }

    val improver = new AssignmentImprover(problem)

    "Improved schedule should be better than the base one" in {
      given Random = new Random(0)
      val schedule = scheduleBase()
      improver.improve(schedule)
      val newScore = schedule.getTotalScore
      newScore should be > baseScore
      println(schedule.toPrettyString)
      println(s"Score: $newScore")
    }

    "Repeated improvements should do nothing (usually)" in {
      given Random = new Random(0)
      val schedule = scheduleBase()
      improver.improve(schedule)
      val newScore = schedule.getTotalScore
      newScore should be > baseScore
      //println(schedule.toPrettyString)
      //println(s"Score: $newScore")
      improver.improve(schedule)
      val newNewScore = schedule.getTotalScore
      newNewScore should be(newScore)
      //println(schedule.toPrettyString)
      //println(s"Score: $newScore")
    }

    "Improved schedule should be consistent" in {
      given Random = new Random(0)
      val schedule = scheduleBase()
      improver.improve(schedule)
      checkConsistency(schedule)
    }

    "Multiple runs should produce the same result" in {
      given Random = new Random(0)

      (0 until 10000).foreach { i =>
        val schedule = scheduleBase()
        improver.improve(schedule)
        (i, schedule.getTotalScore) should be(i -> 14.84375)
      }

    }

  }

  "Swapping persons on linked topics" - {

    "when the linked topics are full" - {
      /* Albert wants to go on Lambda, Charly wants to go on Xi and Upsilon. All topics are full, so the only way to
       * improve is to swap Albert and Charly (or Daniela), on both slots at once. */
      val input: InputModel = InputLoader.fromString(
        """gaston {
          |  settings { default-min-persons-per-topic = 1, default-max-persons-per-topic = 2 }
          |  slots: [ [ {name: "D1"}, {name: "D2"} ] ]
          |  topics: [ {name: "Xi"}, {name: "Upsilon"}, {name: "Lambda", duration: 2} ]
          |  persons: [
          |    {name: "Albert", wishes {"Lambda": 1}},
          |    {name: "Bianca"},
          |    {name: "Charly", wishes {"Xi": 1, "Upsilon": 1}},
          |    {name: "Daniela"}
          |  ]
          |}""".stripMargin
      ).force
      val problem = InputTranscription2(input).result.toEither.force
      val Seq(d1, d2) = problem.slotsCount.range
      val Seq(_, _, xi, upsilon, lambda1, lambda2) = problem.topicsCount.range
      val Seq(albert, bianca, charly, daniela) = problem.personsCount.range

      def scheduleBase() = mkSchedule(problem) {
        d1.slot {
          xi.topic(albert, bianca)
          lambda1.topic(charly, daniela)
        }
        d2.slot {
          upsilon.topic(albert, bianca)
          lambda2.topic(charly, daniela)
        }
      }

      "improves the schedule" in {
        given Random = new Random(0)
        val schedule = scheduleBase()
        improver(problem).improve(schedule)
        checkConsistency(schedule)
        schedule.personsToTopics(albert) should be(SmallIdSet(lambda1, lambda2))
        schedule.getTotalScore should be > Score.Zero
      }
    }

    "when two linked groups share the same slots" - {
      /* Albert and Charly would both be better off swapping (Alpha and Beta are each on both slots). Moving Albert alone
       * doesn't improve the score, as Bianca loses her person-wish on Albert. */
      val input: InputModel = InputLoader.fromString(
        """gaston {
          |  settings { default-min-persons-per-topic = 1, default-max-persons-per-topic = 3 }
          |  slots: [ [ {name: "D1"}, {name: "D2"} ] ]
          |  topics: [ {name: "Alpha", duration: 2}, {name: "Beta", duration: 2} ]
          |  persons: [
          |    {name: "Albert", wishes {"Beta": 1}},
          |    {name: "Bianca", person-wishes {"Albert": 1}},
          |    {name: "Charly", wishes {"Alpha": 1}},
          |    {name: "Daniela"}
          |  ]
          |}""".stripMargin
      ).force
      val problem = InputTranscription2(input).result.toEither.force
      val Seq(d1, d2) = problem.slotsCount.range
      val Seq(_, _, alpha1, alpha2, beta1, beta2) = problem.topicsCount.range
      val Seq(albert, bianca, charly, daniela) = problem.personsCount.range

      def scheduleBase() = mkSchedule(problem) {
        d1.slot {
          alpha1.topic(albert, bianca)
          beta1.topic(charly, daniela)
        }
        d2.slot {
          alpha2.topic(albert, bianca)
          beta2.topic(charly, daniela)
        }
      }

      "keeps the schedule consistent" in {
        given Random = new Random(0)
        val schedule = scheduleBase()
        improver(problem).improve(schedule)
        checkConsistency(schedule)
      }

      "improves the schedule" in {
        given Random = new Random(0)
        val schedule = scheduleBase()
        val baseScore = schedule.getTotalScore
        improver(problem).improve(schedule)
        schedule.personsToTopics(albert) should be(SmallIdSet(beta1, beta2))
        schedule.getTotalScore should be > baseScore
      }
    }

    def improver(problem: SmallProblem) = new AssignmentImprover(problem)
  }

}
