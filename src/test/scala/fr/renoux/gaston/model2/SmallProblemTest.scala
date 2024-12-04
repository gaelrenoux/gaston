package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase
import fr.renoux.gaston.input.InputModel
import fr.renoux.gaston.input.InputLoader
import fr.renoux.gaston.input.InputTranscription2
import fr.renoux.gaston.TestUtils.*
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
        0.topicEmpty
        2 topic (a, d, e) // Alpha, ADE
        6 topic (b, c, f) // Epsilon #1, BCF
        4 topic (g, h, i) // Gamma, GHI
        8 topic (j, k, l) // Eta ~1, JKL
      }
      1 slot {
        1.topicEmpty
        3 topic (b, c, f) // Beta, BCF
        7 topic (a, d, e) // Epsilon #2, ADE,
        5 topic (g, h, i) // Delta, GHI
        9 topic (j, k, l) // Eta ~2, JKL
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
      val betterSchedule = scheduleBase().move(c, epsilon1, alpha)
      problem.scorePersons(betterSchedule).toMap should be(
        personScoresBase + (c -> 500)
      )
      problem.score(betterSchedule) should be(500.0 / 2048)
    }

    "unsatisfied incompatible" in {
      val betterSchedule = scheduleBase().move(a, epsilon2, beta).move(c, beta, epsilon2)
      problem.scorePersons(betterSchedule).toMap should be(
        personScoresBase + (a -> -800) // +200 for the wish, -1000 for the incompatibility
      )
      problem.score(betterSchedule) should be(-800.0)
    }

    "unsatisfied forbidden" in {
      val betterSchedule = scheduleBase().move(e, epsilon2, beta)
      problem.scorePersons(betterSchedule).toMap should be(
        personScoresBase + (e -> Score.MinReward)
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

    // TODO more tests to do

    "person base score" ignore {
      fail()
    }

  }

}
