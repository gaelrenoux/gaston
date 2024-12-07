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
        println(s"Topics count: ${oldProblem.topicsList.size}")
        println(s"Topics: ${oldProblem.topicsList.map(_.name)}")
        val Seq(alpha, beta, gamma, delta, epsilon2, epsilon1, eta1, eta2, theta, unassignedD1, unassignedD2) =
          oldProblem.topicsList
        println(s"Persons count: ${oldProblem.personsList.size}")
        val Seq(a, b, c, d, e, f, g, h, i, j, k, l) = oldProblem.personsList

        old.Schedule.from(
          d1(
            alpha(a, d, e),
            epsilon1(b, c, f),
            gamma(g, h, i),
            eta1(j, k, l)
          ),
          d2(
            beta(b, c, f),
            epsilon2(a, d, e),
            delta(g, h, i),
            eta2(j, k, l)
          )
        )
      }
      val newSchedule: Schedule = ScheduleMaker.fromOldSchedule(oldSchedule, problem, true)
      problem.score(newSchedule) should be(Score.Zero)

      newSchedule.countSlots should be (problem.slotsCount)
      newSchedule.countTopics should be (problem.topicsCount)
      newSchedule.countPersons should be (problem.personsCount)
      println(newSchedule.toPrettyString)
    }
  }

  // TODO More tests required
}
