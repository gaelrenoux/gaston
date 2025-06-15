package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase
import fr.renoux.gaston.TestUtils.*
import fr.renoux.gaston.input.{InputLoader, InputModel, InputTranscription2}
import fr.renoux.gaston.model2.ScheduleMaker.mkSchedule

class SchedulePrinterTest extends TestBase {
  "Printing nominal schedule" in {
    val input: InputModel = InputLoader.fromClassPath("scoring-test.conf").force

    given problem: SmallProblem = InputTranscription2(input).result.toEither.force

    given SchedulePrinter = new SchedulePrinter(problem)

    val Seq(d1, d2) = problem.slotsCount.range
    val Seq(unassigned1, unassigned2, alpha, beta, gamma, delta, epsilon1, epsilon2, eta1, eta2, theta) =
      problem.topicsCount.range
    val Seq(a, b, c, d, e, f, g, h, i, j, k, l) = problem.personsCount.range

    val schedule = mkSchedule(problem) {
      d1.slot {
        unassigned1.topicEmpty
        alpha.topic(a, d, e) // Alpha, ADE
        epsilon1.topic(b, c, f) // Epsilon #1, BCF
        gamma.topic(g, h, i) // Gamma, GHI
        eta1.topic(j, k, l) // Eta ~1, JKL
      }
      d2.slot {
        unassigned2.topicEmpty
        beta.topic(b, c, f) // Beta, BCF
        epsilon2.topic(a, d, e) // Epsilon #2, ADE,
        delta.topic(g, h, i) // Delta, GHI
        eta2.topic(j, k, l) // Eta ~2, JKL
      }
    }

    schedule.toPrettyString should be(
      """D1
        |    @Unassigned (D1) ==>
        |    Alpha ==> Albert, Daniela, Eric
        |    Gamma ==> Galahad, Hypatia, Ignace
        |    Epsilon #1 ==> Bianca, Charly, Fiona
        |    Eta ~1 ==> Joan, Kevin, Laura
        |D2
        |    @Unassigned (D2) ==>
        |    Beta ==> Bianca, Charly, Fiona
        |    Delta ==> Galahad, Hypatia, Ignace
        |    Epsilon #2 ==> Albert, Daniela, Eric
        |    Eta ~2 ==> Joan, Kevin, Laura
        |Unscheduled topics: Theta
        |
        |Total score: 0.0
        |  Pure topics: 0.0
        |  Persons: 0.0
        |    Albert: 0.0
        |    Bianca: 0.0
        |    Charly: 0.0
        |    Daniela: 0.0
        |    Eric: 0.0
        |    Fiona: 0.0
        |    Galahad: 0.0
        |    Hypatia: 0.0
        |    Ignace: 0.0
        |    Joan: 0.0
        |    Kevin: 0.0
        |    Laura: 0.0
        |""".stripMargin)

  }
}
