package fr.renoux.gaston.engine

import java.time.Instant

/**
  * @param stopAtScore Stop any improvement once you reach this value.
  * @param maxImprovementRounds Max number of rounds to try to improve.
  * @param timeout Once reached, stop creating schedules and terminate the runner.
  * @param maxIterations Once reached, stop creating schedules and terminate the runner.
  */
final case class OptimParams( // TODO Rename to Controls, or maybe TerminationConditions? And add class doc.
    stopAtScore: Option[Double] = None,
    maxImprovementRounds: Option[Int] = None, // not exactly a termination condition, but I don't think it's ever used in practice
    timeout: Option[Instant] = None,
    maxIterations: Option[Long] = None
)

object OptimParams {
  val Default: OptimParams = OptimParams()
}
