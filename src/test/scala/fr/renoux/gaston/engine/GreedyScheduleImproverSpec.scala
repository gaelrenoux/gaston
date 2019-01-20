package fr.renoux.gaston.engine

import fr.renoux.gaston.input.{InputSettings, InputLoader}


class GreedyScheduleImproverSpec extends ScheduleImproverAbstractSpec {

  "Greedy improver" should "work a valid schedule (on a complex model)" in {
    val settings: InputSettings = InputLoader.fromDefault.forceToInput.gaston.settings
    val complexTestModel = fr.renoux.gaston.ComplexTestModel(42L)(settings)
    val (_, bestScore) = runWith(new GreedyScheduleImprover(_), complexTestModel.Problems.Complete, 0L until 1L)
    bestScore should be >= 6300.0
  }

  it should "work a valid schedule (on a real-life model)" in {
    val problem = InputLoader.fromClassPath("udocon-2017-completed.conf").forceToModel
    val (_, bestScore) = runWith(new GreedyScheduleImprover(_), problem, 0L until 1L)
    bestScore should be >= 2200.0
  }

}
