package fr.renoux.gaston.model2

import scala.annotation.targetName

opaque type Score >: Double = Double

object Score {

  extension (s: Score) {
    @targetName("scorePlusScore")
    infix inline def +(t: Score): Score = s + t

    @targetName("scoreMultiplyWeight")
    infix inline def *(w: Weight): Score = w * s
  }

  inline def Zero: Score = 0.0

  /** A minimum reward that won't overflow when it's summed with others */
  inline def MinReward: Score = -1e9

  given Ordering[Score] = math.Ordering.Double.TotalOrdering

  given Printable[Score] with {
    extension (a: Score) override def toPrettyString: String = a.toString
  }
}

opaque type Weight >: Double = Double

object Weight {
  extension (w: Weight) {
    // TODO When Scala 3 has fixed https://github.com/scala/scala3/issues/17158, this can go back to just being *
    @targetName("weightMultiplyScore")
    infix inline def *(s: Score): Score = w * s
  }

  given Printable[Weight] with {
    extension (a: Weight) override def toPrettyString: String = a.toString
  }
}
