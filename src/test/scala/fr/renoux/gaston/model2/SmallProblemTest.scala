package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase
import fr.renoux.gaston.input.InputModel
import fr.renoux.gaston.input.InputLoader
import fr.renoux.gaston.input.InputTranscription2
import fr.renoux.gaston.TestUtils.*
import com.softwaremill.quicklens.*
import ScheduleMaker.mkSchedule


class SmallProblemTest extends TestBase {

  // TODO test copy

  "Scoring" - {
    val input: InputModel = InputLoader.fromClassPath("scoring-test.conf").force
    val problem = InputTranscription2(input).result.toEither.force
    import problem.given
    given SchedulePrinter = new SchedulePrinter(problem)

    val Seq(unassigned0, unassigned1, alpha, beta, gamma, delta, epsilon1, epsilon2, eta1, eta2, theta) =
      problem.topicsCount.range
    val Seq(a, b, c, d, e, f, g, h, i, j, k, l) = problem.personsCount.range
    def scheduleBase() = mkSchedule {
      0 slot {
        unassigned0.topicEmpty
        alpha topic (a, d, e) // Alpha, ADE
        epsilon1 topic (b, c, f) // Epsilon #1, BCF
        gamma topic (g, h, i) // Gamma, GHI
        eta1 topic (j, k, l) // Eta ~1, JKL
      }
      1 slot {
        unassigned1.topicEmpty
        beta topic (b, c, f) // Beta, BCF
        epsilon2 topic (a, d, e) // Epsilon #2, ADE,
        delta topic (g, h, i) // Delta, GHI
        eta2 topic (j, k, l) // Eta ~2, JKL
      }
    }
    println(scheduleBase().toPrettyString)
    val personScoresBase: Map[PersonId, Score] = problem.personsCount.range.map(_ -> Score.Zero).toMap

    "basic schedule" in {
      val schedule = scheduleBase()
      problem.scorePersons(schedule).valuesSeq should be(Seq.fill(problem.personsCount.value)(Score.Zero))
      problem.score(schedule) should be(Score.Zero)
    }

    "satisfied wish" in {
      val betterSchedule = scheduleBase().move(f, epsilon1, alpha)
      problem.scorePersons(betterSchedule).toMap should be(
        personScoresBase + (f -> 750)
      )
      problem.score(betterSchedule) should be(750.0 / 2048)
    }

    "satisfied person-wish" in {
      val betterSchedule = scheduleBase().move(h, delta, beta).move(h, gamma, alpha)
      problem.scorePersons(betterSchedule).toMap should be(
        personScoresBase + (c -> 500)
      )
      problem.score(betterSchedule) should be(500.0 / 2048)
    }

    "person base score" in {
      val schedule = scheduleBase()
      val input2 =
        input.modify(_.persons.eachWhere(_.name == "Bianca").baseScore).setTo(fr.renoux.gaston.model.Score(100))
      val problem2 = InputTranscription2(input2).problem
      problem2.scorePersons(schedule).toMap should be(
        personScoresBase + (b -> 100)
      )
      problem2.score(schedule) should be(100.0 / 2048)
    }

    "topic base score" in {
      val betterSchedule = scheduleBase().addTopic(1, theta)
      problem.scorePersons(betterSchedule).toMap should be(personScoresBase)
      problem.score(betterSchedule) should be(20)
    }

    "unsatisfied incompatible" in {
      val betterSchedule = scheduleBase().move(g, gamma, alpha).move(g, delta, beta)
      problem.scorePersons(betterSchedule).toMap should be(
        personScoresBase + (a -> -1000)
      )
      problem.score(betterSchedule) should be(-1000.0)
    }

    "unsatisfied forbidden" in {
      val betterSchedule = scheduleBase().move(e, epsilon2, beta)
      problem.scorePersons(betterSchedule).toMap should be(
        personScoresBase + (e -> Score.MinReward)
      )
      problem.score(betterSchedule) should be(Score.MinReward)
    }

    "unsatisfied exclusive (on unassigned)" in {
      val betterSchedule = scheduleBase().move(a, alpha, unassigned0).move(a, epsilon2, unassigned1)
      problem.scorePersons(betterSchedule).toMap should be(
        personScoresBase + (a -> (-100 - 100 - 50)) // two unassigned, plus the exclusive constraint
      )
      problem.score(betterSchedule) should be(-250)
    }

    "unsatisfied exclusive (on occurrences)" in {
      val betterSchedule = scheduleBase().move(a, alpha, epsilon1)
      problem.scorePersons(betterSchedule).toMap should be(
        personScoresBase + (a -> Score.MinReward)
      )
      problem.score(betterSchedule) should be(Score.MinReward)
    }

    "unsatisfied linked" in {
      val betterSchedule = scheduleBase().move(j, eta1, alpha)
      problem.scorePersons(betterSchedule).toMap should be(
        personScoresBase + (j -> Score.MinReward)
      )
      problem.score(betterSchedule) should be(Score.MinReward)
    }

    "weight is considered" in {
      val betterSchedule = scheduleBase().move(d, epsilon2, beta)
      problem.scorePersons(betterSchedule).toMap should be(
        personScoresBase + (d -> 500)
      )
      problem.score(betterSchedule) should be(500.0 / 2048)
    }
    // TODO test weight on other scores as well

  }

}
