package fr.renoux.gaston.engine

import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.input._

class ExhaustiveScheduleImproverSpec extends ScheduleImproverAbstractSpec {

  "Systematic improver" should "work a valid schedule (on a complex model)" in {
    val complexTestModel = fr.renoux.gaston.ComplexTestModel(42L)
    val (_, bestScore) = runWith(new ExhaustiveScheduleImprover(_), complexTestModel.Problems.Complete, 0L until 1L)
    bestScore should be >= 12.0
  }

  it should "work a valid schedule (on a real-life model)" in {
    val problem = problemFromClassPath("udocon-2017-completed.conf").force
    val (_, bestScore) = runWith(new ExhaustiveScheduleImprover(_), problem, 0L until 1L)
    bestScore should be >= 430.0
  }
}
