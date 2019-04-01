package fr.renoux.gaston.engine

import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.input._

class GreedyScheduleImproverSpec extends ScheduleImproverAbstractSpec {

  "Fast improver" should "work a valid schedule (on a complex model)" in {
    val complexTestModel = fr.renoux.gaston.ComplexTestModel(42L)
    val (_, bestScore) = runWith(new GreedyScheduleImprover(_), complexTestModel.Problems.Complete, 0L until 1L)
    bestScore should be >= 12.0
  }

  it should "work a valid schedule (on a real-life model)" in {
    val problem = problemFromClassPath("udocon-2017-completed.conf").force
    val (_, bestScore) = runWith(new GreedyScheduleImprover(_), problem, 4L until 5L)
    bestScore should be >= 300.0
  }

}
