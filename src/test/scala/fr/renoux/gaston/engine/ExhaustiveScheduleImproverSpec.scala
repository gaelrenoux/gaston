package fr.renoux.gaston.engine

import fr.renoux.gaston.input.InputLoader

class ExhaustiveScheduleImproverSpec extends ScheduleImproverAbstractSpec {

  "Systematic improver" should "work a valid schedule (on a complex model)" in {
    val complexTestModel = fr.renoux.gaston.ComplexTestModel(42L)
    val (_, bestScore) = runWith(new ExhaustiveScheduleImprover(_), complexTestModel.Problems.Complete, 0L until 1L)
    bestScore should be >= 12.0
  }

  it should "work a valid schedule (on a real-life model)" in {
    val problem = InputLoader.fromClassPath("udocon-2017-completed.conf").forceToModel
    val (_, bestScore) = runWith(new ExhaustiveScheduleImprover(_), problem, 0L until 1L)
    bestScore should be >= 430.0
  }
}
