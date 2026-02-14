package fr.renoux.gaston.model2

import com.softwaremill.quicklens.*
import fr.renoux.gaston.TestBase
import fr.renoux.gaston.TestUtils.*
import fr.renoux.gaston.input.{InputLoader, InputModel, InputTranscription2}
import fr.renoux.gaston.model2.ScheduleMaker.mkSchedule

class ScheduleTest extends TestBase {
  // TODO test move/swap individually
  // TODO missing tests on SlotAssignment stuff

  object Model {
    val input: InputModel = InputLoader.fromClassPath("scoring-test.conf").force

    given problem: SmallProblem = InputTranscription2(input).result.toEither.force

    given SchedulePrinter = new SchedulePrinter(problem)

    val Seq(d1, d2) = problem.slotsCount.range
    val Seq(unassigned0, unassigned1, alpha, beta, gamma, delta, epsilon1, epsilon2, eta1, eta2, theta) =
      problem.topicsCount.range
    val Seq(alf, bab, cat, dan, eli, fay, guy, han, ing, jon, kal, luc) = problem.personsCount.range

    def scheduleBase(myProblem: SmallProblem = problem) = mkSchedule(myProblem) {
      d1.slot {
        unassigned0.topicEmpty
        alpha.topic(alf, dan, eli) // Alpha, ADE
        epsilon1.topic(bab, cat, fay) // Epsilon #1, BCF
        gamma.topic(guy, han, ing) // Gamma, GHI
        eta1.topic(jon, kal, luc) // Eta ~1, JKL
      }
      d2.slot {
        unassigned1.topicEmpty
        beta.topic(bab, cat, fay) // Beta, BCF
        epsilon2.topic(alf, dan, eli) // Epsilon #2, ADE,
        delta.topic(guy, han, ing) // Delta, GHI
        eta2.topic(jon, kal, luc) // Eta ~2, JKL
      }
    }

    println(scheduleBase().toPrettyString)
    val personScoresBase: Map[PersonId, Score] = problem.personsCount.range.map(_ -> Score.Zero).toMap
  }

  "Basics " - {
    given countSlots: CountAll[SlotId] = CountAll[SlotId](2)

    given countTopics: CountAll[TopicId] = CountAll[TopicId](4)

    given countPersons: CountAll[PersonId] = CountAll[PersonId](4)

    val baseProblem = SmallProblemMaker.make

    import ScheduleMaker.mkSchedule

    val schedule = mkSchedule(baseProblem) {
      0.slot {
        0.topic(0, 1, 2)
        1.topic(3)
      }
      1.slot {
        3.topic(0, 1, 2, 3)
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

    "equals" in {
      (schedule == schedule) should be(true)
      val expected = mkSchedule(baseProblem) {
        0.slot {
          0.topic(0, 1, 2)
          1.topic(3)
        }
        1.slot {
          3.topic(0, 1, 2, 3)
        }
      }
      (schedule == expected) should be(true)
      (expected == schedule) should be(true)

      val notExpected = mkSchedule(baseProblem) {
        0.slot {
          0.topic(0, 1, 3)
          1.topic(2)
        }
        1.slot {
          3.topic(0, 1, 2, 3)
        }
      }
      (schedule == notExpected) should be(false)
      (notExpected == schedule) should be(false)
    }

    "SlotAssignment.equals" in {
      (schedule.slotsToAssignment(0) == schedule.slotsToAssignment(0)) should be(true)
      val expected = mkSchedule(baseProblem) {
        0.slot {
          0.topic(0, 1, 2)
          1.topic(3)
        }
        1.slot {
          3.topic(0, 1, 2, 3)
        }
      }
      (schedule.slotsToAssignment(0) == expected.slotsToAssignment(0)) should be(true)
      (expected.slotsToAssignment(0) == schedule.slotsToAssignment(0)) should be(true)

      val notExpected = mkSchedule(baseProblem) {
        0.slot {
          0.topic(0, 1, 3)
          1.topic(2)
        }
        1.slot {
          3.topic(0, 1, 2, 3)
        }
      }
      (schedule.slotsToAssignment(0) == notExpected.slotsToAssignment(0)) should be(false)
      (notExpected.slotsToAssignment(0) == schedule.slotsToAssignment(0)) should be(false)
    }

  }

  "Scoring" - {
    import Model.{*, given}

    "basic schedule" in {
      val schedule = scheduleBase()
      schedule.getPersonsToScore.valuesSeq should be(Seq.fill(problem.personsCount.value)(Score.Zero))
      schedule.getTotalScore should be(Score.Zero)
    }

    "satisfied wish" in {
      val betterSchedule = scheduleBase().on(d1)(_.move(fay, epsilon1, alpha))
      betterSchedule.recalculateScoreForPersonsPendingChanges()
      betterSchedule.getPersonsToScore.toMap should be(
        personScoresBase + (fay -> 750)
      )
      betterSchedule.getTotalScore should be(750.0 / 2048)
    }

    "satisfied person-wish" in {
      val betterSchedule = scheduleBase().on(d2)(_.move(han, delta, beta)).on(d1)(_.move(han, gamma, alpha))
      betterSchedule.recalculateScoreForPersonsPendingChanges()
      betterSchedule.getPersonsToScore.toMap should be(
        personScoresBase + (cat -> 500)
      )
      betterSchedule.getTotalScore should be(500.0 / 2048)
    }

    "person base score" in {
      val input2 =
        input.modify(_.persons.eachWhere(_.name == "Bianca").baseScore).setTo(fr.renoux.gaston.model.Score(100))
      val problem2 = InputTranscription2(input2).problem
      val schedule = scheduleBase(problem2)
      schedule.getPersonsToScore.toMap should be(
        personScoresBase + (bab -> 100)
      )
      schedule.getTotalScore should be(100.0 / 2048)
    }

    "topic base score" in {
      val betterSchedule = scheduleBase().addTopic(1, theta)
      betterSchedule.getPersonsToScore.toMap should be(personScoresBase)
      betterSchedule.getTotalScore should be(20)
    }

    "unsatisfied incompatible" in {
      val betterSchedule = scheduleBase().on(d1)(_.move(guy, gamma, alpha)).on(d2)(_.move(guy, delta, beta))
      betterSchedule.recalculateScoreForPersonsPendingChanges()
      betterSchedule.getPersonsToScore.toMap should be(
        personScoresBase + (alf -> -1000)
      )
      betterSchedule.getTotalScore should be(-1000.0)
    }

    "unsatisfied forbidden" in {
      val betterSchedule = scheduleBase().on(d2)(_.move(eli, epsilon2, beta))
      betterSchedule.recalculateScoreForPersonsPendingChanges()
      betterSchedule.getPersonsToScore.toMap should be(
        personScoresBase + (eli -> Score.MinReward)
      )
      betterSchedule.getTotalScore should be(Score.MinReward)
    }

    "unsatisfied exclusive (on unassigned)" in {
      val betterSchedule = scheduleBase().on(d1)(_.move(alf, alpha, unassigned0)).on(d2)(_.move(alf, epsilon2, unassigned1))
      betterSchedule.recalculateScoreForPersonsPendingChanges()
      betterSchedule.getPersonsToScore.toMap should be(
        personScoresBase + (alf -> (-100 - 100 - 50)) // two unassigned, plus the exclusive constraint
      )
      betterSchedule.getTotalScore should be(-250)
    }

    "unsatisfied exclusive (on occurrences)" in {
      val betterSchedule = scheduleBase().on(d1)(_.move(alf, alpha, epsilon1))
      betterSchedule.recalculateScoreForPersonsPendingChanges()
      betterSchedule.getPersonsToScore.toMap should be(
        personScoresBase + (alf -> Score.MinReward)
      )
      betterSchedule.getTotalScore should be(Score.MinReward)
    }

    "unsatisfied linked" in {
      val betterSchedule = scheduleBase().on(d1)(_.move(jon, eta1, alpha))
      betterSchedule.recalculateScoreForPersonsPendingChanges()
      betterSchedule.getPersonsToScore.toMap should be(
        personScoresBase + (jon -> Score.MinReward)
      )
      betterSchedule.getTotalScore should be(Score.MinReward)
    }

    "weight is considered" in {
      val betterSchedule = scheduleBase().on(d2)(_.move(dan, epsilon2, beta))
      betterSchedule.recalculateScoreForPersonsPendingChanges()
      betterSchedule.getPersonsToScore.toMap should be(
        personScoresBase + (dan -> 500)
      )
      betterSchedule.getTotalScore should be(500.0 / 2048)
    }
    // TODO test weight on other scores as well

    "multiple moves in a row recalculate the score correctly" in {
      val schedule = scheduleBase()
      schedule.getTotalScore should be(Score.Zero)
      schedule.on(d1)(_.move(fay, epsilon1, alpha))
      schedule.recalculateScoreForPersonsPendingChanges()
      schedule.getTotalScore should be(750.0 / 2048)
      schedule.on(d1)(_.move(han, gamma, alpha))
      schedule.on(d2)(_.move(han, delta, beta))
      schedule.recalculateScoreForPersonsPendingChanges()
      schedule.getTotalScore should be(750.0 / 2048 + 500.0 / 1024)
    }
  }

  "Undoing" - {
    import Model.*

    "undoing and restores bring the schedule back to its initial stage" in {
      val schedule = scheduleBase()
      schedule.getTotalScore should be(Score.Zero)
      schedule.saveScores()

      schedule.on(d1)(_.move(fay, epsilon1, alpha))
      schedule.recalculateScoreForPersonsPendingChanges()
      schedule.getTotalScore should be(750.0 / 2048)

      schedule.on(d1)(_.undoMove(fay, epsilon1, alpha))
      schedule.restoreSavedScores()
      schedule.getTotalScore should be(Score.Zero)
      schedule should be(scheduleBase())
    }

    "undoing and restores bring the schedule back to its initial stage (more complex)" in {
      val schedule = scheduleBase()
      schedule.getTotalScore should be(Score.Zero)
      schedule.saveScores()

      // Some basic changes
      schedule.on(d1)(_.move(han, gamma, alpha))
      schedule.on(d2)(_.move(han, delta, beta))
      schedule.recalculateScoreForPersonsPendingChanges()
      schedule.getTotalScore should be(500.0 / 2048)

      // Move and undo, on a simple case
      schedule.saveScores()
      schedule.on(d1)(_.move(fay, epsilon1, alpha))
      schedule.recalculateScoreForPersonsPendingChanges()
      schedule.getTotalScore should be(750.0 / 2048 + 500.0 / 1024)
      schedule.on(d1)(_.undoMove(fay, epsilon1, alpha))
      schedule.restoreSavedScores()
      schedule.getTotalScore should be(500.0 / 2048)

      // Free I out of linked topics
      schedule.on(d1)(_.move(ing, gamma, epsilon1))
      schedule.on(d2)(_.move(ing, delta, beta))
      schedule.recalculateScoreForPersonsPendingChanges()
      schedule.getTotalScore should be(500.0 / 2048)

      // One savepoint
      schedule.saveScores()

      // Move and undo, with temporary incompatibility
      schedule.on(d1)(_.move(ing, epsilon1, alpha))
      schedule.recalculateScoreForPersonsPendingChanges()
      schedule.getTotalScore should be(500.0 / 2048 - 1000) // incompatible with A
      schedule.on(d1)(_.undoMove(ing, epsilon1, alpha))
      schedule.restoreSavedScores()
      schedule.getTotalScore should be(500.0 / 2048)

      // Simple change to force recalculation on A
      schedule.on(d2)(_.move(alf, epsilon2, beta))
      schedule.recalculateScoreForPersonsPendingChanges()
      schedule.getTotalScore should be(500.0 / 2048 - 800) // A got a wish and an incompatibility
      schedule.on(d2)(_.undoMove(alf, epsilon2, beta))
      schedule.restoreSavedScores()
      schedule.getTotalScore should be(500.0 / 2048)

      // Swap and undo, changing the global score (not just the slot score)
      schedule.on(d2)(_.swap(dan, epsilon2, guy, delta))
      schedule.recalculateScoreForPersonsPendingChanges()
      schedule.getTotalScore.value should be < (-1E9) // because Gamma and Delta are linked
      schedule.on(d2)(_.undoSwap(dan, epsilon2, guy, delta))
      schedule.restoreSavedScores()
      schedule.getTotalScore should be(500.0 / 2048)

      // One last change to force recalculation on D
      schedule.on(d2)(_.move(dan, epsilon2, beta))
      schedule.recalculateScoreForPersonsPendingChanges()
      schedule.getTotalScore should be(500.0 / 2048 + 500.0 / 1024)
    }
  }

}
