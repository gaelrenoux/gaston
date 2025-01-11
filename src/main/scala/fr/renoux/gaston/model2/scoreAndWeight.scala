package fr.renoux.gaston.model2

import scala.annotation.targetName

opaque type Score >: Double = Double

object Score {

  extension (s: Score) {
    @targetName("scorePlusScore")
    infix inline def +(t: Score): Score = s + t

    @targetName("scoreMultiplyWeight")
    infix inline def *(w: Weight): Score = w * s
    
    @targetName("scoreMultiplyInt")
    infix inline def *(i: Int): Score = i * s

    @targetName("scoreSuperior")
    infix inline def >(t: Score): Boolean = s > t

    @targetName("scoreSuperiorOrEqual")
    infix inline def >=(t: Score): Boolean = s >= t

    @targetName("scoreInferior")
    infix inline def <(t: Score): Boolean = s < t

    @targetName("scoreInferiorOrEqual")
    infix inline def <=(t: Score): Boolean = s <= t

    inline def value: Double = s
  }

  inline def Zero: Score = 0.0

  /** A minimum reward that won't overflow when it's summed with others */
  inline def MinReward: Score = -1e9

  /** Some value to represent no value (in caches and stuff) */
  inline def Missing: Score = Double.MinValue

  given Ordering[Score] = math.Ordering.Double.TotalOrdering

  given Printable[Score] with {
    extension (a: Score) override def toPrettyString: String = a.toString
  }
}

opaque type Weight >: Double = Double

object Weight {
  extension (w: Weight) {
    @targetName("weightMultiplyScore")
    infix inline def *(s: Score): Score = w * s

    inline def value: Double = w
  }

  inline def Default: Weight = 1.0

  given Printable[Weight] with {
    extension (a: Weight) override def toPrettyString: String = a.toString
  }
}
