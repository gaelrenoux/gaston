package fr.renoux.gaston.model

import cats.Monoid

/** A Score evaluate how much a preference or a set of preferences is satisfied. The higher the better. Scores are added
 * when combining preferences. Negative preferences ("I'd rather not...") have a negative score when triggered. */
opaque type Score = Double

object Score {

  extension (s: Score) {

    inline def value: Double = s

    /** Adds two scores */
    inline def +(t: Score): Score = s + t

    /** Subtract two scores */
    inline def -(t: Score): Score = Score(s - t)

    /** Multiply a score by a constant factor */
    inline def *(i: Int): Score = s * i

    /** Multiply a score by a constant factor */
    inline def *(l: Long): Score = s * l

    /** Multiply a score by a constant factor */
    inline def *(d: Double): Score = s * d

    /** Divide a score by a weight */
    inline def /(w: Weight): Score = s / w

    inline def opposite: Score = -s

    inline def toFormattedString: String = Score.TwoDecimalsFormat.format(s)

    inline def isNegativeInfinity: Boolean = s.isNegInfinity

    inline def isPositive: Boolean = s > 0
  }

  def apply(s: Double): Score = s

  private val TwoDecimalsFormat = new java.text.DecimalFormat("####.00")

  val Zero: Score = Score(0)

  val MinValue: Score = Score(Double.MinValue)

  val NegativeInfinity: Score = Score(Double.NegativeInfinity)

  given Ordering[Score] with {
    override def compare(x: Score, y: Score): Int = x.compareTo(y)
  }

  given Monoid[Score] with {
    override val empty: Score = Score.Zero

    override def combine(a: Score, b: Score): Score = a + b
  }


  /** Implementation of the Fractional typeclass for Score */
  given Fractional[Score] with {
    override def plus(x: Score, y: Score): Score = Score(x.value + y.value)

    override def minus(x: Score, y: Score): Score = Score(x.value - y.value)

    override def times(x: Score, y: Score): Score = Score(x.value * y.value)

    override def div(x: Score, y: Score): Score = Score(x.value / y.value)

    override def negate(x: Score): Score = Score(-x.value)

    override def fromInt(x: Int): Score = Score(x.toDouble)

    override def toInt(x: Score): Int = x.value.toInt

    override def toLong(x: Score): Long = x.value.toLong

    override def toFloat(x: Score): Float = x.value.toFloat

    override def toDouble(x: Score): Double = x.value.toDouble

    override def compare(x: Score, y: Score): Int = x.value.compareTo(y.value)

    override def abs(x: Score): Score = Score(math.abs(x.value))

    override def parseString(str: String): Option[Score] = str.toDoubleOption
  }

  def sum[A](it: Iterable[A])(f: A => Score): Score =
    if (it.isEmpty) Score.Zero
    else it.foldLeft(Score.Zero)(_ + f(_))

}

/** The higher the weight of a person, the more its preferences matter when calculating a score for a set of preferences
 * involving multiple persons. */
opaque type Weight = Double

object Weight {
  extension (w: Weight) {
    inline def value: Double = w
  }

  def apply(s: Double): Weight = if (s > 0) s else throw new IllegalArgumentException

  val Default: Weight = 1.0
}
