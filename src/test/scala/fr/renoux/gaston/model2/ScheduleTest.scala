package fr.renoux.gaston.model2

import com.softwaremill.quicklens.*
import fr.renoux.gaston.TestBase
import fr.renoux.gaston.TestUtils.*
import fr.renoux.gaston.input.{InputLoader, InputModel, InputTranscription2}
import fr.renoux.gaston.model2.ScheduleMaker.mkSchedule

class ScheduleTest extends TestBase {
  // TODO test move/swap individually
  // TODO missing tests on SlotAssignment stuff

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
        1. slot {
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
          1.topic (2)
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
    val input: InputModel = InputLoader.fromClassPath("scoring-test.conf").force

    given problem: SmallProblem = InputTranscription2(input).result.toEither.force

    given SchedulePrinter = new SchedulePrinter(problem)

    val Seq(d1, d2) = problem.slotsCount.range
    val Seq(unassigned0, unassigned1, alpha, beta, gamma, delta, epsilon1, epsilon2, eta1, eta2, theta) =
      problem.topicsCount.range
    val Seq(a, b, c, d, e, f, g, h, i, j, k, l) = problem.personsCount.range

    def scheduleBase(myProblem: SmallProblem = problem) = mkSchedule(myProblem) {
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

    println(scheduleBase().toPrettyString)
    val personScoresBase: Map[PersonId, Score] = problem.personsCount.range.map(_ -> Score.Zero).toMap

    "basic schedule" in {
      val schedule = scheduleBase()
      schedule.getPersonsToScore().valuesSeq should be(Seq.fill(problem.personsCount.value)(Score.Zero))
      schedule.getTotalScore() should be(Score.Zero)
    }

    "satisfied wish" in {
      val betterSchedule = scheduleBase().on(d1)(_.move(f, epsilon1, alpha))
      betterSchedule.getPersonsToScore().toMap should be(
        personScoresBase + (f -> 750)
      )
      betterSchedule.getTotalScore() should be(750.0 / 2048)
    }

    "satisfied person-wish" in {
      val betterSchedule = scheduleBase().on(d2)(_.move(h, delta, beta)).on(d1)(_.move(h, gamma, alpha))
      betterSchedule.getPersonsToScore().toMap should be(
        personScoresBase + (c -> 500)
      )
      betterSchedule.getTotalScore() should be(500.0 / 2048)
    }

    "person base score" in {
      val input2 =
        input.modify(_.persons.eachWhere(_.name == "Bianca").baseScore).setTo(fr.renoux.gaston.model.Score(100))
      val problem2 = InputTranscription2(input2).problem
      val schedule = scheduleBase(problem2)
      schedule.getPersonsToScore().toMap should be(
        personScoresBase + (b -> 100)
      )
      schedule.getTotalScore() should be(100.0 / 2048)
    }

    "topic base score" in {
      val betterSchedule = scheduleBase().addTopic(1, theta)
      betterSchedule.getPersonsToScore().toMap should be(personScoresBase)
      betterSchedule.getTotalScore() should be(20)
    }

    "unsatisfied incompatible" in {
      val betterSchedule = scheduleBase().on(d1)(_.move(g, gamma, alpha)).on(d2)(_.move(g, delta, beta))
      betterSchedule.getPersonsToScore().toMap should be(
        personScoresBase + (a -> -1000)
      )
      betterSchedule.getTotalScore() should be(-1000.0)
    }

    "unsatisfied forbidden" in {
      val betterSchedule = scheduleBase().on(d2)(_.move(e, epsilon2, beta))
      betterSchedule.getPersonsToScore().toMap should be(
        personScoresBase + (e -> Score.MinReward)
      )
      betterSchedule.getTotalScore() should be(Score.MinReward)
    }

    "unsatisfied exclusive (on unassigned)" in {
      val betterSchedule = scheduleBase().on(d1)(_.move(a, alpha, unassigned0)).on(d2)(_.move(a, epsilon2, unassigned1))
      betterSchedule.getPersonsToScore().toMap should be(
        personScoresBase + (a -> (-100 - 100 - 50)) // two unassigned, plus the exclusive constraint
      )
      betterSchedule.getTotalScore() should be(-250)
    }

    "unsatisfied exclusive (on occurrences)" in {
      val betterSchedule = scheduleBase().on(d1)(_.move(a, alpha, epsilon1))
      betterSchedule.getPersonsToScore().toMap should be(
        personScoresBase + (a -> Score.MinReward)
      )
      betterSchedule.getTotalScore() should be(Score.MinReward)
    }

    "unsatisfied linked" in {
      val betterSchedule = scheduleBase().on(d1)(_.move(j, eta1, alpha))
      betterSchedule.getPersonsToScore().toMap should be(
        personScoresBase + (j -> Score.MinReward)
      )
      betterSchedule.getTotalScore() should be(Score.MinReward)
    }

    "weight is considered" in {
      val betterSchedule = scheduleBase().on(d2)(_.move(d, epsilon2, beta))
      betterSchedule.getPersonsToScore().toMap should be(
        personScoresBase + (d -> 500)
      )
      betterSchedule.getTotalScore() should be(500.0 / 2048)
    }
    // TODO test weight on other scores as well


    "Stored score behavior" - {
      "multiple moves in a row recalculate the score correctly" in {
        val schedule = scheduleBase()
        schedule.getTotalScore() should be(Score.Zero)
        schedule.on(d1)(_.move(f, epsilon1, alpha))
        schedule.getTotalScore() should be(750.0 / 2048)
        schedule.on(d1)(_.move(h, gamma, alpha))
        schedule.on(d2)(_.move(h, delta, beta))
        schedule.getTotalScore() should be(750.0 / 2048 + 500.0 / 1024)
      }

      "undoing restores the score correctly" in {
        val schedule = scheduleBase()
        schedule.getTotalScore() should be(Score.Zero)
        schedule.on(d1)(_.move(f, epsilon1, alpha))
        schedule.getTotalScore() should be(750.0 / 2048)
        schedule.on(d1)(_.undoMove(f, epsilon1, alpha))
        schedule.getTotalScore() should be(Score.Zero)
      }

      "undoing restores the score correctly (more complex)" in {
        val schedule = scheduleBase()

        // Some basic changes
        schedule.getTotalScore() should be(Score.Zero)
        schedule.on(d1)(_.move(h, gamma, alpha))
        schedule.on(d2)(_.move(h, delta, beta))
        schedule.getTotalScore() should be(500.0 / 2048)

        // Move and undo, on a simple case
        schedule.on(d1)(_.move(f, epsilon1, alpha))
        schedule.getTotalScore() should be(750.0 / 2048 + 500.0 / 1024)
        schedule.on(d1)(_.undoMove(f, epsilon1, alpha))
        schedule.getTotalScore() should be(500.0 / 2048)

        // Free I out of linked topics
        schedule.on(d1)(_.move(i, gamma, epsilon1))
        schedule.on(d2)(_.move(i, delta, beta))
        schedule.getTotalScore() should be(500.0 / 2048)

        // Move and undo, with temporary incompatibility
        schedule.on(d1)(_.move(i, epsilon1, alpha))
        schedule.getTotalScore() should be(500.0 / 2048 - 1000) // incompatible with A
        schedule.on(d1)(_.undoMove(i, epsilon1, alpha))
        schedule.getTotalScore() should be(500.0 / 2048)

        // Simple change to force recalculation on A
        schedule.on(d2)(_.move(a, epsilon2, beta))
        schedule.getTotalScore() should be(500.0 / 2048 - 800) // A got a wish and an incompatibility
        schedule.on(d2)(_.undoMove(a, epsilon2, beta))
        schedule.getTotalScore() should be(500.0 / 2048)

        // Swap and undo, changing the global score (not just the slot score)
        schedule.on(d2)(_.swap(d, epsilon2, g, delta))
        schedule.getTotalScore().value should be < (-1E9) // because Gamma and Delta are linked
        schedule.on(d2)(_.undoSwap(d, epsilon2, g, delta))
        schedule.getTotalScore() should be(500.0 / 2048)

        // One last change to force recalculation on D
        schedule.on(d2)(_.move(d, epsilon2, beta))
        schedule.getTotalScore() should be(500.0 / 2048 + 500.0 / 1024)
      }

    }
  }

}
