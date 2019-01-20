package fr.renoux.gaston.engine

import fr.renoux.gaston.input.{InputSettings, PureConfigLoader}

class SystematicScheduleImproverSpec extends ScheduleImproverAbstractSpec {

  "Systematic improver" should "work a valid schedule (on a complex model)" in {
    val settings: InputSettings = PureConfigLoader.fromClassPath.forceToInput.gaston.settings
    val complexTestModel = fr.renoux.gaston.ComplexTestModel(42L)(settings)
    val (_, bestScore) = runWith(new SystematicScheduleImprover(_), complexTestModel.Problems.Complete, 0L until 1L)
    bestScore should be > 6300.0
  }

  it should "work a valid schedule (on a real-life model)" in {
    val problem = PureConfigLoader.fromClassPath("udocon-2017-completed.conf").forceToModel
    val (_, bestScore) = runWith(new SystematicScheduleImprover(_), problem, 0L until 1L)
    bestScore should be > 2200.0
  }
}
