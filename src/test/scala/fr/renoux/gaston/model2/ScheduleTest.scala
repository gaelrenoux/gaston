package fr.renoux.gaston.model2

import com.softwaremill.quicklens.*
import fr.renoux.gaston.TestBase
import fr.renoux.gaston.TestUtils.*
import fr.renoux.gaston.input.{InputLoader, InputModel, InputTranscription2}
import fr.renoux.gaston.model2.ScheduleMaker.mkSchedule
import fr.renoux.gaston.util.fastLoop
import org.scalatest.Assertion

import scala.util.Random

class ScheduleTest extends TestBase {

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

    "hashCode is consistent with equals" in {
      val expected = mkSchedule(baseProblem) {
        0.slot {
          0.topic(0, 1, 2)
          1.topic(3)
        }
        1.slot {
          3.topic(0, 1, 2, 3)
        }
      }
      schedule should be(expected)
      schedule.hashCode should be(expected.hashCode)
    }

    "scheduledTopics on each assignment matches slotsToTopics, including empty topics" in {
      val scheduleWithEmptyTopics = mkSchedule(baseProblem) {
        0.slot {
          0.topic(0, 1, 2, 3)
          1.topicEmpty
        }
        1.slot {
          2.topicEmpty
          3.topicEmpty
        }
      }
      countSlots.range.foreach { sid =>
        scheduleWithEmptyTopics.slotsToAssignment(sid).scheduledTopics should be(scheduleWithEmptyTopics.slotsToTopics(sid))
      }
    }

    "addTopic updates the assignment's scheduledTopics" in {
      val s = mkSchedule(baseProblem) {
        0.slot {
          0.topic(0, 1, 2)
          1.topic(3)
        }
        1.slot {
          3.topic(0, 1, 2, 3)
        }
      }
      s.addTopic(1, 2)
      s.slotsToTopics(1).toSet should be(Set(2, 3))
      s.slotsToAssignment(1).scheduledTopics should be(s.slotsToTopics(1))
    }

    "removeTopic updates the assignment's scheduledTopics" in {
      val s = mkSchedule(baseProblem) {
        0.slot {
          0.topic(0, 1, 2, 3)
          1.topicEmpty
        }
        1.slot {
          3.topic(0, 1, 2, 3)
        }
      }
      s.removeTopic(1)
      s.slotsToTopics(0).toSet should be(Set(0))
      s.slotsToAssignment(0).scheduledTopics should be(s.slotsToTopics(0))
    }

  }

  "Moves and swaps" - {
    import Model.*

    "move updates all indexes" in {
      val schedule = scheduleBase().on(d1)(_.move(fay, epsilon1, alpha))
      val assignment = schedule.slotsToAssignment(d1)
      assignment.personsToTopic(fay) should be(alpha)
      assignment.topicsToPersons(alpha) should be(SmallIdSet(alf, dan, eli, fay))
      assignment.topicsToPersons(epsilon1) should be(SmallIdSet(bab, cat))
      schedule.personsToTopics(fay) should be(SmallIdSet(alpha, beta))
      schedule.slowCheckup should be(Nil)
    }

    "undoMove restores all indexes" in {
      val schedule = scheduleBase().on(d1)(_.move(fay, epsilon1, alpha)).on(d1)(_.undoMove(fay, epsilon1, alpha))
      schedule should be(scheduleBase())
      schedule.personsToTopics should be(scheduleBase().personsToTopics)
      schedule.slotsToAssignment(d1).topicsToPersons should be(scheduleBase().slotsToAssignment(d1).topicsToPersons)
    }

    "swap updates all indexes" in {
      val schedule = scheduleBase().on(d2)(_.swap(alf, epsilon2, bab, beta))
      val assignment = schedule.slotsToAssignment(d2)
      assignment.personsToTopic(alf) should be(beta)
      assignment.personsToTopic(bab) should be(epsilon2)
      assignment.topicsToPersons(beta) should be(SmallIdSet(alf, cat, fay))
      assignment.topicsToPersons(epsilon2) should be(SmallIdSet(bab, dan, eli))
      schedule.personsToTopics(alf) should be(SmallIdSet(alpha, beta))
      schedule.personsToTopics(bab) should be(SmallIdSet(epsilon1, epsilon2))
      schedule.slowCheckup should be(Nil)
    }

    "undoSwap restores all indexes" in {
      val schedule = scheduleBase().on(d2)(_.swap(alf, epsilon2, bab, beta)).on(d2)(_.undoSwap(alf, epsilon2, bab, beta))
      schedule should be(scheduleBase())
      schedule.personsToTopics should be(scheduleBase().personsToTopics)
      schedule.slotsToAssignment(d2).topicsToPersons should be(scheduleBase().slotsToAssignment(d2).topicsToPersons)
    }
  }

  "Incremental scoring" - {
    import Model.{*, given}

    /** Checks the incrementally-maintained scores against a full recalculation (which also resets them). */
    def checkAgainstFullRecalculation(schedule: Schedule): Assertion = {
      val incrementalTotalScore = schedule.getTotalScore
      val incrementalPersonsScores = schedule.getPersonsToScore.valuesSeq.toList // copy, as the underlying array is mutable
      schedule.recalculateAll()
      schedule.getPersonsToScore.valuesSeq.toList should be(incrementalPersonsScores)
      schedule.getTotalScore should be(incrementalTotalScore)
    }

    "matches a full recalculation over random moves, swaps and undos" in {
      given rand: Random = new Random(0)

      val schedule = scheduleBase()
      fastLoop(0, 1000) { _ =>
        val sid: SlotId = problem.slotsCount.random
        val assignment = schedule.slotsToAssignment(sid)
        val pid1: PersonId = problem.personsCount.random
        val pid2: PersonId = problem.personsCount.random
        val tid1 = assignment.personsToTopic(pid1)
        val tid2 = assignment.personsToTopic(pid2)
        if (tid1 != tid2) {
          val isSwap = rand.nextBoolean()
          val isUndone = rand.nextBoolean()
          schedule.saveScores()
          if (isSwap) {
            val _ = assignment.swap(pid1, tid1, pid2, tid2)
          } else {
            val _ = assignment.move(pid1, tid1, tid2)
          }
          schedule.recalculateScoreForPersonsPendingChanges()
          if (isUndone) {
            if (isSwap) {
              val _ = assignment.undoSwap(pid1, tid1, pid2, tid2)
            } else {
              val _ = assignment.undoMove(pid1, tid1, tid2)
            }
            schedule.restoreSavedScores()
          }
          schedule.slowCheckup should be(Nil)
          val _ = checkAgainstFullRecalculation(schedule)
        }
      }
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
