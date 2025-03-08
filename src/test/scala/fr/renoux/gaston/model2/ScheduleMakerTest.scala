package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase
import fr.renoux.gaston.input.InputModel
import fr.renoux.gaston.input.problemFromClassPath
import fr.renoux.gaston.input.InputTranscription2
import fr.renoux.gaston.{model as old}
import fr.renoux.gaston.input.InputLoader
import fr.renoux.gaston.TestUtils.*
import fr.renoux.gaston.util.Context


class ScheduleMakerTest extends TestBase {
  given Context = Context.Debug


  "on basic problem" - {

    val input: InputModel = InputLoader.fromClassPath("scoring-test.conf").force

    val problem = InputTranscription2(input).result.toEither.force

    given Printable[Schedule] = SchedulePrinter(problem)

    val Seq(day1, day2) = problem.slotsCount.range
    val Seq(unassigned1, unassigned2, alpha, beta, gamma, delta, epsilon1, epsilon2, eta1, eta2, theta) = problem.topicsCount.range
    val Seq(albert, bianca, charly, daniela, eric, fiona, galahad, hypatia, ignace, joan, kevin, laura) = problem.personsCount.range

    "mkSchedule" in {
      import problem.given
      val schedule = ScheduleMaker.mkSchedule(problem) {
        day1 slot {
          unassigned1.topic (eric)
          alpha.topic(albert, daniela)
          epsilon1.topic(bianca, charly, fiona)
          gamma.topic(galahad, hypatia, ignace)
          eta1.topic(joan, kevin, laura)
        }
        day2 slot {
          unassigned2.topic (fiona)
          beta.topic(bianca, charly)
          epsilon2.topic(albert, daniela, eric)
          delta.topic(galahad, hypatia, ignace)
          eta2.topic(joan, kevin, laura)
        }
      }

      val expectedTopicsToSlot = IdMap(
        unassigned1 -> day1, alpha -> day1, epsilon1 -> day1, gamma -> day1, eta1 -> day1,
        unassigned2 -> day2, beta -> day2, epsilon2 -> day2, delta -> day2, eta2 -> day2,
        theta -> SlotId.None
      )
      schedule.topicsToSlot should be(expectedTopicsToSlot)

      val expectedPersonsToTopics = IdMap(
        albert -> SmallIdSet(alpha, epsilon2),
        bianca -> SmallIdSet(beta, epsilon1),
        charly -> SmallIdSet(beta, epsilon1),
        daniela -> SmallIdSet(alpha, epsilon2),
        eric -> SmallIdSet(epsilon2, unassigned1),
        fiona -> SmallIdSet(epsilon1, unassigned2),
        galahad -> SmallIdSet(delta, gamma),
        hypatia -> SmallIdSet(delta, gamma),
        ignace -> SmallIdSet(delta, gamma),
        joan -> SmallIdSet(eta1, eta2),
        kevin -> SmallIdSet(eta1, eta2),
        laura -> SmallIdSet(eta1, eta2)
      )
      schedule.personsToTopics should be(expectedPersonsToTopics)

      val assignment1 = schedule.slotsToAssignment(day1)
      assignment1.slot should be(day1)
      val expectedPersonsToTopicsDay1 = IdMap(
        eric -> unassigned1,
        albert -> alpha, daniela -> alpha,
        bianca -> epsilon1, charly -> epsilon1, fiona -> epsilon1,
        galahad -> gamma, hypatia -> gamma, ignace -> gamma,
        joan -> eta1, kevin -> eta1, laura -> eta1
      )
      assignment1.personsToTopic should be(expectedPersonsToTopicsDay1)
      val expectedTopicsToPersonsDay1 = IdMap(
        unassigned1 -> SmallIdSet(eric),
        alpha -> SmallIdSet(albert, daniela),
        epsilon1 -> SmallIdSet(bianca, charly, fiona),
        gamma -> SmallIdSet(galahad, hypatia, ignace),
        eta1 -> SmallIdSet(joan, kevin, laura)
      )
      assignment1.topicsToPersons should be(expectedTopicsToPersonsDay1)

      val assignment2 = schedule.slotsToAssignment(day2)
      assignment2.slot should be(day2)
      val expectedPersonsToTopicsDay2 = IdMap(
        fiona -> unassigned2,
        bianca -> beta, charly -> beta,
        albert -> epsilon2, daniela -> epsilon2, eric -> epsilon2,
        galahad -> delta, hypatia -> delta, ignace -> delta,
        joan -> eta2, kevin -> eta2, laura -> eta2
      )
      assignment2.personsToTopic should be(expectedPersonsToTopicsDay2)
      val expectedTopicsToPersonsDay2 = IdMap(
        unassigned2 -> SmallIdSet(fiona),
        beta -> SmallIdSet(bianca, charly),
        epsilon2 -> SmallIdSet(albert, daniela, eric),
        delta -> SmallIdSet(galahad, hypatia, ignace),
        eta2 -> SmallIdSet(joan, kevin, laura)
      )
      assignment2.topicsToPersons should be(expectedTopicsToPersonsDay2)
    }

    "fromOldSchedule" in {

      given oldProblem: old.Problem = problemFromClassPath("scoring-test.conf").force
      val Seq(oldDay1, oldDay2) = oldProblem.slotsList
      val Seq(oldAlpha, oldBeta, oldGamma, oldDelta, oldEpsilon2, oldEpsilon1, oldEta1, oldEta2, oldTheta, oldUnassigned1, oldUnassigned2) = oldProblem.topicsList
      val Seq(a, b, c, d, e, f, g, h, i, j, k, l) = oldProblem.personsList

      val oldSchedule: old.Schedule = {

        old.Schedule.from(
          oldDay1(
            oldUnassigned1(e),
            oldAlpha(a, d),
            oldEpsilon1(b, c, f),
            oldGamma(g, h, i),
            oldEta1(j, k, l)
          ),
          oldDay2(
            oldUnassigned2(),
            oldBeta(b, c, f),
            oldEpsilon2(a, d, e),
            oldDelta(g, h, i),
            oldEta2(j, k, l)
          )
        )
      }

      val convertedSchedule: Schedule = ScheduleMaker.fromOldSchedule(oldSchedule, problem, true)

      val expectedSchedule = {
        ScheduleMaker.mkSchedule(problem) {
          day1.slot {
            unassigned1.topic(eric)
            alpha.topic(albert, daniela)
            epsilon1.topic(bianca, charly, fiona)
            gamma.topic(galahad, hypatia, ignace)
            eta1.topic(joan, kevin, laura)
          }
          day2 slot {
            unassigned2.topicEmpty
            beta.topic(bianca, charly, fiona)
            epsilon2.topic(albert, daniela, eric)
            delta.topic(galahad, hypatia, ignace)
            eta2.topic(joan, kevin, laura)
          }
        }
      }

      convertedSchedule.slotsToAssignment(0) should be(expectedSchedule.slotsToAssignment(0))
      convertedSchedule.slotsToAssignment(1) should be(expectedSchedule.slotsToAssignment(1))
      convertedSchedule.personsToTopics should be(expectedSchedule.personsToTopics)
      convertedSchedule.topicsToSlot should be(expectedSchedule.topicsToSlot)
      convertedSchedule should be(expectedSchedule)
    }
  }

  // TODO More tests required
}
