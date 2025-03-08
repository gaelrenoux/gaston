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

  "fromOldSchedule" - {

    "basic problem" in {
      val input: InputModel = InputLoader.fromClassPath("scoring-test.conf").force

      given oldProblem: old.Problem = problemFromClassPath("scoring-test.conf").force

      val problem = InputTranscription2(input).result.toEither.force

      given Printable[Schedule] = SchedulePrinter(problem)

      val oldSchedule: old.Schedule = {
        val Seq(d1, d2) = oldProblem.slotsList
        val Seq(alpha, beta, gamma, delta, epsilon2, epsilon1, eta1, eta2, theta, unassigned1, unassigned2) = oldProblem.topicsList
        val Seq(a, b, c, d, e, f, g, h, i, j, k, l) = oldProblem.personsList

        old.Schedule.from(
          d1(
            unassigned1(),
            alpha(a, d, e),
            epsilon1(b, c, f),
            gamma(g, h, i),
            eta1(j, k, l)
          ),
          d2(
            unassigned2(),
            beta(b, c, f),
            epsilon2(a, d, e),
            delta(g, h, i),
            eta2(j, k, l)
          )
        )
      }

      val convertedSchedule: Schedule = ScheduleMaker.fromOldSchedule(oldSchedule, problem, true)
      convertedSchedule.getTotalScore() should be(Score.Zero)

      val expectedSchedule = {
        val Seq(d1, d2) = problem.slotsCount.range
        val Seq(alpha, beta, gamma, delta, epsilon2, epsilon1, eta1, eta2, theta, unassigned1, unassigned2) = problem.topicsCount.range
        val Seq(a, b, c, d, e, f, g, h, i, j, k, l) = problem.personsCount.range

        ScheduleMaker.mkSchedule(problem) {
          d1 slot {
            // unassigned1 topic ()
            alpha topic(a, d, e)
            epsilon1 topic(b, c, f)
            gamma topic(g, h, i)
            eta1 topic(j, k, l)
          }
          d2 slot {
            // unassigned2 topic ()
            beta topic(b, c, f)
            epsilon2 topic(a, d, e)
            delta topic(g, h, i)
            eta2 topic(j, k, l)
          }
        }
      }

      convertedSchedule should be(expectedSchedule)
    }
  }

  // TODO More tests required
}
