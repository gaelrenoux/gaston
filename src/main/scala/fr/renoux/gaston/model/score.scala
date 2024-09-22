package fr.renoux.gaston.model

import cats.Monoid


/** A Score evaluate how nuch a preference or a set of preferences is satisfied. The higher, the better. Negative
  * preferences ("I'd rather not...") have a negative score when triggered.
  *
  * Scores can be of three different types: flat, log, and exp. Scores of different types cannot be directly compared.
  * Logarithmic and exponential scores are passed through the corresponding function after being summed on each person
  * (non-personal log or exp scores make no sense). So, increases on log values have progressively less impact, while
  * increases on exp values have progressively more. (Note that the actual calculation over the log/exp scores are a bit
  * more complicated to normalize them over all persons.)
  *
  * Once the functions have been applied, all we're left with is a flat score.
  *
  * Typically, log scores are positive (for things that are very good once but where multiples are less great) and exp
  * scores are negative (for things that are OK once but very bad if multiple).
  */
sealed trait Score {
  val value: Double // the flat value of the score

  @inline def ++(s: Score): Score = (this, s) match {
    case (fs1: FlatScore, fs2: FlatScore) => fs1 + fs2
  }

  @inline def toFormattedString: String = Score.TwoDecimalsFormat.format(value)
}

object Score {
  private val TwoDecimalsFormat = new java.text.DecimalFormat("####.00")
}

final case class AggregatedScore(flat: Score, log: Score, exp: Score) extends Score {
  val value = 0.0 // TODO
}

final case class FlatScore(value: Double) extends AnyVal with Score with Ordered[FlatScore] {

  /** Adds two scores */
  @inline def +(s: FlatScore): FlatScore = FlatScore(value + s.value)

  /** Subtract two scores */
  def -(s: FlatScore): FlatScore = FlatScore(value - s.value)

  /** Multiply a score by a constant factor */
  @inline def *(i: Int): FlatScore = FlatScore(value * i)

  /** Multiply a score by a constant factor */
  @inline def *(l: Long): FlatScore = FlatScore(value * l)

  /** Multiply a score by a constant factor */
  @inline def *(d: Double): FlatScore = FlatScore(value * d)

  /** Divide a score by a weight */
  @inline def /(w: Weight): FlatScore = FlatScore(value / w.value)

  @inline def negative: FlatScore = FlatScore(-value)

  @inline override def compare(that: FlatScore): Int = this.value.compare(that.value)

  @inline def isNegativeInfinity: Boolean = value.isNegInfinity

  @inline def isPositive: Boolean = value > 0
}

object FlatScore extends (Double => FlatScore) {

  /** What score should a person have if all its preferences are satisfied ? */
  val PersonTotalScore: FlatScore = FlatScore(1000.0)

  val Zero: FlatScore = FlatScore(0)

  val MinValue: FlatScore = FlatScore(Double.MinValue)

  val NegativeInfinity: FlatScore = FlatScore(Double.NegativeInfinity)

  implicit object FlatScoreIsMonoid extends Monoid[FlatScore] {
    override val empty: FlatScore = FlatScore.Zero

    override def combine(a: FlatScore, b: FlatScore): FlatScore = a + b
  }


  /** Implementation of the Fractional typeclass for Score */
  implicit object FlatScoreIsFractional extends Fractional[FlatScore] {
    override def plus(x: FlatScore, y: FlatScore): FlatScore = FlatScore(x.value + y.value)

    override def minus(x: FlatScore, y: FlatScore): FlatScore = FlatScore(x.value - y.value)

    override def times(x: FlatScore, y: FlatScore): FlatScore = FlatScore(x.value * y.value)

    override def div(x: FlatScore, y: FlatScore): FlatScore = FlatScore(x.value / y.value)

    override def negate(x: FlatScore): FlatScore = FlatScore(-x.value)

    override def fromInt(x: Int): FlatScore = FlatScore(x.toDouble)

    override def toInt(x: FlatScore): Int = x.value.toInt

    override def toLong(x: FlatScore): Long = x.value.toLong

    override def toFloat(x: FlatScore): Float = x.value.toFloat

    override def toDouble(x: FlatScore): Double = x.value.toDouble

    override def compare(x: FlatScore, y: FlatScore): Int = x.value.compareTo(y.value)

    override def abs(x: FlatScore): FlatScore = FlatScore(math.abs(x.value))

    override def parseString(str: String): Option[FlatScore] = str.toDoubleOption.map(FlatScore)
  }

  def sum[A](it: Iterable[A])(f: A => FlatScore): FlatScore =
    if (it.isEmpty) FlatScore.Zero
    else FlatScore(it.foldLeft(0.0)(_ + f(_).value))

}


final case class PositiveLogScore(value: Double) extends AnyVal with Score with Ordered[PositiveLogScore] {
  assert(value >= 0)

  /** Adds two scores */
  @inline def +(s: PositiveLogScore): PositiveLogScore = PositiveLogScore(value + s.value)

  /** Multiply a score by a constant factor */
  @inline def *(i: Int): PositiveLogScore = PositiveLogScore(value * i)

  /** Multiply a score by a constant factor */
  @inline def *(l: Long): PositiveLogScore = PositiveLogScore(value * l)

  /** Multiply a score by a constant factor */
  @inline def *(d: Double): PositiveLogScore = PositiveLogScore(value * d)

  @inline override def compare(that: PositiveLogScore): Int = this.value.compare(that.value)
}

object PositiveLogScore extends (Double => PositiveLogScore) {

  val Zero: PositiveLogScore = PositiveLogScore(0)

  implicit object PositiveLogScoreIsMonoid extends Monoid[PositiveLogScore] {
    override val empty: PositiveLogScore = PositiveLogScore.Zero

    override def combine(a: PositiveLogScore, b: PositiveLogScore): PositiveLogScore = a + b
  }

  def sum[A](it: Iterable[A])(f: A => PositiveLogScore): PositiveLogScore =
    if (it.isEmpty) PositiveLogScore.Zero
    else PositiveLogScore(it.foldLeft(0.0)(_ + f(_).value))

}


final case class NegativeExpScore(value: Double) extends AnyVal with Score with Ordered[NegativeExpScore] {
  assert(value <= 0)

  /** Adds two scores */
  @inline def +(s: NegativeExpScore): NegativeExpScore = NegativeExpScore(value + s.value)

  /** Multiply a score by a constant factor */
  @inline def *(i: Int): NegativeExpScore = NegativeExpScore(value * i)

  /** Multiply a score by a constant factor */
  @inline def *(l: Long): NegativeExpScore = NegativeExpScore(value * l)

  /** Multiply a score by a constant factor */
  @inline def *(d: Double): NegativeExpScore = NegativeExpScore(value * d)

  @inline override def compare(that: NegativeExpScore): Int = this.value.compare(that.value)
}

object NegativeExpScore extends (Double => NegativeExpScore) {

  val Zero: NegativeExpScore = NegativeExpScore(0)

  implicit object PositiveLogScoreIsMonoid extends Monoid[NegativeExpScore] {
    override val empty: NegativeExpScore = NegativeExpScore.Zero

    override def combine(a: NegativeExpScore, b: NegativeExpScore): NegativeExpScore = a + b
  }

  def sum[A](it: Iterable[A])(f: A => NegativeExpScore): NegativeExpScore =
    if (it.isEmpty) NegativeExpScore.Zero
    else NegativeExpScore(it.foldLeft(0.0)(_ + f(_).value))

}

