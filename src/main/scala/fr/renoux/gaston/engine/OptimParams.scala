package fr.renoux.gaston.engine

import java.time.Instant

case class OptimParams(
    stopAtScore: Option[Double] = None,
    maxImprovementRounds: Option[Int] = None,
    timeout: Option[Instant] = None
)

object OptimParams {
  val Default = OptimParams()
}