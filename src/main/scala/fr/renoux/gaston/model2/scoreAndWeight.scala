package fr.renoux.gaston.model2

import scala.annotation.targetName


opaque type Score >: Double = Double

extension (s: Score) {
  @targetName("plus")
  infix inline def +(t: Score): Score = s + t

  @targetName("multiplyWeight")
  infix inline def *(w: Weight): Score = w * s
}

object Score {
  inline def Zero: Score = 0.0

  /** A minimum reward that won't overflow when it's summed with others */
  inline def MinReward: Score = -1e9

  given Ordering[Score] = math.Ordering.Double.TotalOrdering
}

opaque type Weight >: Double = Double

extension (w: Weight) {
  @targetName("multiplyScore")
  infix inline def *(s: Score): Score = w * s
}
