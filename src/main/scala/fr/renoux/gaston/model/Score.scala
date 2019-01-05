package fr.renoux.gaston.model

import java.text.DecimalFormat

/** A Score evaluate how nuch a preference or a set of preferences is satisfied. The higher the better. Scores are added
  * when combining preferences. Negative preferences ("I'd rather not…") have a negative score when triggered. */
case class Score(value: Double) extends AnyVal with Ordered[Score] {

  /** Adds two scores */
  def +(s: Score) = Score(value + s.value)

  /** Multiply a score by a constant factor */
  def *(i: Int) = Score(value * i)

  /** Multiply a score by a constant factor */
  def *(l: Long) = Score(value * l)

  /** Divide a score by a weight */
  def /(w: Weight) = Score(value / w.value)

  def negative = Score(-value)

  override def compare(that: Score): Int = this.value.compare(that.value)

  def toFormattedString: String = Score.TwoDecimalsFormat.format(value)
}

object Score {

  private val TwoDecimalsFormat = new DecimalFormat("####.00")

  val Zero = Score(0)

  val MinValue = Score(Double.MinValue)

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
  }

}