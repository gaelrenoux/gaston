package fr.renoux.gaston.model2

import scala.annotation.targetName


opaque type Score >: Double = Double

extension (s: Score) {
  // TODO When Scala 3 has fixed https://github.com/scala/scala3/issues/17158, those can go back to just being + and *
  @targetName("scorePlusScore")
  infix inline def <+>(t: Score): Score = s + t

  @targetName("scoreMultiplyWeight")
  infix inline def <*>(w: Weight): Score = w * s
}

object Score {
  inline def Zero: Score = 0.0

  /** A minimum reward that won't overflow when it's summed with others */
  inline def MinReward: Score = -1e9

  given Ordering[Score] = math.Ordering.Double.TotalOrdering
}

opaque type Weight >: Double = Double

extension (w: Weight) {
  // TODO When Scala 3 has fixed https://github.com/scala/scala3/issues/17158, this can go back to just being *
  @targetName("weightMultiplyScore")
  infix inline def <*>(s: Score): Score = w * s
}
