package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase
import fr.renoux.gaston.input.InputModel
import fr.renoux.gaston.input.InputLoader
import fr.renoux.gaston.input.InputTranscription2
import fr.renoux.gaston.TestUtils.*
import com.softwaremill.quicklens.*
import ScheduleMaker.mkSchedule

class ScheduleTest extends TestBase {
  // TODO test move/swap individually

  "Basics " - { // TODO fix that, we need a way to create a simple problem
    given countSlots: CountAll[SlotId] = CountAll[SlotId](2)
    given countTopics: CountAll[TopicId] = CountAll[TopicId](4)
    given countPersons: CountAll[PersonId] = CountAll[PersonId](4)

    val baseProblem = SmallProblemMaker.make

    import ScheduleMaker.mkSchedule

    val schedule = mkSchedule(baseProblem) {
      0 slot {
        0 topic(0, 1, 2)
        1 topic (3)
      }
      1 slot {
        3 topic(0, 1, 2, 3)
      }
    }

    "slotsToTopics" in {
      val stt = schedule.slotsToTopics.toMap.view.mapValues(_.toSet).toMap
      stt should be(Map(0 -> Set(0, 1), 1 -> Set(3)))
    }

    "topicsToSlots" in {
      val tts = schedule.topicsToSlot.toMap
      tts should be(Map(0 -> 0, 1 -> 0, 2 -> SlotId.None, 3 -> 1))
    }

    "personsToTopics" in {
      val ptp = schedule.personsToTopics.toMap.view.mapValues(_.toSet).toMap
      ptp should be(Map(0 -> Set(0, 3), 1 -> Set(0, 3), 2 -> Set(0, 3), 3 -> Set(1, 3)))
    }

    "topicsPresent" in {
      val tp = schedule.topicsPresent.toSet
      tp should be(Set(0, 1, 3))
    }
  }

  "Scoring" - {
    val input: InputModel = InputLoader.fromClassPath("scoring-test.conf").force

    given problem: SmallProblem = InputTranscription2(input).result.toEither.force
    import problem.given
    given SchedulePrinter = new SchedulePrinter(problem)

    val Seq(d1, d2) = problem.slotsCount.range
    val Seq(unassigned0, unassigned1, alpha, beta, gamma, delta, epsilon1, epsilon2, eta1, eta2, theta) =
      problem.topicsCount.range
    val Seq(a, b, c, d, e, f, g, h, i, j, k, l) = problem.personsCount.range

    def scheduleBase(myProblem: SmallProblem = problem) = mkSchedule(myProblem) {
      d1 slot {
        unassigned0.topicEmpty
        alpha topic(a, d, e) // Alpha, ADE
        epsilon1 topic(b, c, f) // Epsilon #1, BCF
        gamma topic(g, h, i) // Gamma, GHI
        eta1 topic(j, k, l) // Eta ~1, JKL
      }
      d2 slot {
        unassigned1.topicEmpty
        beta topic(b, c, f) // Beta, BCF
        epsilon2 topic(a, d, e) // Epsilon #2, ADE,
        delta topic(g, h, i) // Delta, GHI
        eta2 topic(j, k, l) // Eta ~2, JKL
      }
    }

    println(scheduleBase().toPrettyString)
    val personScoresBase: Map[PersonId, Score] = problem.personsCount.range.map(_ -> Score.Zero).toMap

    "basic schedule" in {
      val schedule = scheduleBase()
      schedule.slowGetPersonScores().valuesSeq should be(Seq.fill(problem.personsCount.value)(Score.Zero))
      schedule.getTotalScore() should be(Score.Zero)
    }

    "satisfied wish" in {
      val betterSchedule = scheduleBase().on(d1)(_.move(f, epsilon1, alpha))
      betterSchedule.slowGetPersonScores().toMap should be(
        personScoresBase + (f -> 750)
      )
      betterSchedule.getTotalScore() should be(750.0 / 2048)
    }

    "satisfied person-wish" in {
      val betterSchedule = scheduleBase().on(d2)(_.move(h, delta, beta)).on(d1)(_.move(h, gamma, alpha))
      betterSchedule.slowGetPersonScores().toMap should be(
        personScoresBase + (c -> 500)
      )
      betterSchedule.getTotalScore() should be(500.0 / 2048)
    }

    "person base score" in {
      val input2 =
        input.modify(_.persons.eachWhere(_.name == "Bianca").baseScore).setTo(fr.renoux.gaston.model.Score(100))
      val problem2 = InputTranscription2(input2).problem
      val schedule = scheduleBase(problem2)
      schedule.slowGetPersonScores().toMap should be(
        personScoresBase + (b -> 100)
      )
      schedule.getTotalScore() should be(100.0 / 2048)
    }

    "topic base score" in {
      val betterSchedule = scheduleBase().addTopic(1, theta)
      betterSchedule.slowGetPersonScores().toMap should be(personScoresBase)
      betterSchedule.getTotalScore() should be(20)
    }

    "unsatisfied incompatible" in {
      val betterSchedule = scheduleBase().on(d1)(_.move(g, gamma, alpha)).on(d2)(_.move(g, delta, beta))
      betterSchedule.slowGetPersonScores().toMap should be(
        personScoresBase + (a -> -1000)
      )
      betterSchedule.getTotalScore() should be(-1000.0)
    }

    "unsatisfied forbidden" in {
      val betterSchedule = scheduleBase().on(d2)(_.move(e, epsilon2, beta))
      betterSchedule.slowGetPersonScores().toMap should be(
        personScoresBase + (e -> Score.MinReward)
      )
      betterSchedule.getTotalScore() should be(Score.MinReward)
    }

    "unsatisfied exclusive (on unassigned)" in {
      val betterSchedule = scheduleBase().on(d1)(_.move(a, alpha, unassigned0)).on(d2)(_.move(a, epsilon2, unassigned1))
      betterSchedule.slowGetPersonScores().toMap should be(
        personScoresBase + (a -> (-100 - 100 - 50)) // two unassigned, plus the exclusive constraint
      )
      betterSchedule.getTotalScore() should be(-250)
    }

    "unsatisfied exclusive (on occurrences)" in {
      val betterSchedule = scheduleBase().on(d1)(_.move(a, alpha, epsilon1))
      betterSchedule.slowGetPersonScores().toMap should be(
        personScoresBase + (a -> Score.MinReward)
      )
      betterSchedule.getTotalScore() should be(Score.MinReward)
    }

    "unsatisfied linked" in {
      val betterSchedule = scheduleBase().on(d1)(_.move(j, eta1, alpha))
      betterSchedule.slowGetPersonScores().toMap should be(
        personScoresBase + (j -> Score.MinReward)
      )
      betterSchedule.getTotalScore() should be(Score.MinReward)
    }

    "weight is considered" in {
      val betterSchedule = scheduleBase().on(d2)(_.move(d, epsilon2, beta))
      betterSchedule.slowGetPersonScores().toMap should be(
        personScoresBase + (d -> 500)
      )
      betterSchedule.getTotalScore() should be(500.0 / 2048)
    }
    // TODO test weight on other scores as well


    "Cache behavior" - {
      "multiple moves in a row reset cache correctly" in {
        val schedule = scheduleBase()
        schedule.getTotalScore() should be(Score.Zero)
        schedule.on(d1)(_.move(f, epsilon1, alpha))
        schedule.getTotalScore() should be(750.0 / 2048)
        schedule.on(d1)(_.move(h, gamma, alpha))
        schedule.on(d2)(_.move(h, delta, beta))
        schedule.getTotalScore() should be(750.0 / 2048 + 500.0 / 1024)
      }

      "undoing restores the cache correctly" in {
        val schedule = scheduleBase()
        schedule.getTotalScore() should be(Score.Zero)
        schedule.on(d1)(_.move(f, epsilon1, alpha))
        schedule.getTotalScore() should be(750.0 / 2048)
        schedule.on(d1)(_.undoMove(f, epsilon1, alpha))
        schedule.getTotalScore() should be(Score.Zero)
      }

      "undoing restores the cache correctly (more complex)" in {
        val schedule = scheduleBase()
        schedule.getTotalScore() should be(Score.Zero)
        schedule.on(d1)(_.move(h, gamma, alpha))
        schedule.on(d2)(_.move(h, delta, beta))
        schedule.getTotalScore() should be(500.0 / 2048)
        schedule.on(d1)(_.move(f, epsilon1, alpha))
        schedule.getTotalScore() should be(750.0 / 2048 + 500.0 / 1024)

        schedule.on(d1)(_.undoMove(f, epsilon1, alpha))
        schedule.getTotalScore() should be(500.0 / 2048)
      }
    }
  }

}
