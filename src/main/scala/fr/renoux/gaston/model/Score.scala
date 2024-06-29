package fr.renoux.gaston.model

import cats.Monoid


/** A Score evaluate how nuch a preference or a set of preferences is satisfied. The higher the better. Scores are added
  * when combining preferences. Negative preferences ("I'd rather not...") have a negative score when triggered. */
final case class Score(value: Double) extends AnyVal with Ordered[Score] {

  /** Adds two scores */
  @inline def +(s: Score): Score = Score(value + s.value)

  /** Subtract two scores */
  def -(s: Score): Score = Score(value - s.value)

  /** Multiply a score by a constant factor */
  @inline def *(i: Int): Score = Score(value * i)

  /** Multiply a score by a constant factor */
  @inline def *(l: Long): Score = Score(value * l)

  /** Multiply a score by a constant factor */
  @inline def *(d: Double): Score = Score(value * d)

  /** Divide a score by a weight */
  @inline def /(w: Weight): Score = Score(value / w.value)

  @inline def negative: Score = Score(-value)

  @inline override def compare(that: Score): Int = this.value.compare(that.value)

  @inline def toFormattedString: String = Score.TwoDecimalsFormat.format(value)

  @inline def isNegativeInfinity: Boolean = value.isNegInfinity

  @inline def isPositive: Boolean = value > 0
}

object Score extends (Double => Score) {

  private val TwoDecimalsFormat = new java.text.DecimalFormat("####.00")

  /** What score should a person have if all its preferences are satisfied ? */
  val PersonTotalScore: Score = Score(1000.0)

  val Zero: Score = Score(0)

  val MinValue: Score = Score(Double.MinValue)

  val NegativeInfinity: Score = Score(Double.NegativeInfinity)


  implicit object ScoreIsMonoid extends Monoid[Score] {
    override val empty: Score = Score.Zero

    override def combine(a: Score, b: Score): Score = a + b
  }


  /** Implementation of the Fractional typeclass for Score */
  implicit object ScoreIsFractional extends Fractional[Score] {
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

    override def parseString(str: String): Option[Score] = str.toDoubleOption.map(Score)
  }

  def sum[A](it: Iterable[A])(f: A => Score): Score =
    if (it.isEmpty) Score.Zero
    else Score(it.foldLeft(0.0)(_ + f(_).value))

}
