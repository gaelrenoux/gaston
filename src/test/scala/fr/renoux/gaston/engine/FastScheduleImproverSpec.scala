package fr.renoux.gaston.engine

import fr.renoux.gaston.input.InputLoader


class FastScheduleImproverSpec extends ScheduleImproverAbstractSpec {

  "Fast improver" should "work a valid schedule (on a complex model)" in {
    val complexTestModel = fr.renoux.gaston.ComplexTestModel(42L)
    val (_, bestScore) = runWith(new FastScheduleImprover(_), complexTestModel.Problems.Complete, 0L until 1L)
    bestScore should be >= 12.0
  }

  it should "work a valid schedule (on a real-life model)" in {
    val problem = InputLoader.fromClassPath("udocon-2017-completed.conf").forceToModel
    val (_, bestScore) = runWith(new FastScheduleImprover(_), problem, 4L until 5L)
    bestScore should be >= 300.0
  }

}
