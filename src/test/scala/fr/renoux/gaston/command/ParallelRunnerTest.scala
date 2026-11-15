package fr.renoux.gaston.command

import fr.renoux.gaston.engine.{Engine, GreedyEngine, StopSignal, Termination}
import fr.renoux.gaston.model.Problem
import fr.renoux.gaston.util.Context
import fr.renoux.gaston.{SimpleTestModel, TestBase}

class ParallelRunnerTest extends TestBase {

  private given Context = Context.Default

  "Termination" - {
    "On stop signal" in {
      given Problem = SimpleTestModel.Problems.WithUnassignedTopics
      given Engine = new GreedyEngine
      given Output = Output.silent
      val stopSignal = new StopSignal
      val runner = new ParallelRunner(seed = 0, parallelism = 2)

      val stopper = new Thread(() => {
        Thread.sleep(500)
        stopSignal.request()
      })
      stopper.start()

      val startMs = System.currentTimeMillis()
      val (bestSchedule, _) = runner.run(Termination(stopSignal = Some(stopSignal)))
      val durationMs = System.currentTimeMillis() - startMs

      durationMs should be < 5000L
      bestSchedule.isSolution should be(true)
    }
  }
}
