package fr.renoux.gaston

sealed abstract class ScoringStrategy

object ScoringStrategy {
  /** Maximize the minimum score. Personal scores are divided by the person's weight before comparison. The total
    * score comes only as a tie breaker. */
  object MiniMax extends ScoringStrategy

  /** Maximize the sum of all scores. May sacrifice someone to get the best possible score. */
  object Sum extends ScoringStrategy
}