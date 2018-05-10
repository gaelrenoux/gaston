package fr.renoux.gaston.model

case class Score(value: Double) extends AnyVal with Ordered[Score] {

  def +(s: Score) = Score(value + s.value)
  def +(i: Int) = Score(value + i)
  def +(l: Long) = Score(value + l)

  def *(w: Weight) = Score(value * w.value)
  def *(i: Int) = Score(value * i)
  def *(l: Long) = Score(value * l)

  def /(w: Weight) = Score(value / w.value)

  def negative = Score(-value)

  override def compare(that: Score): Int = this.value.compare(that.value)
}

object Score {

  val Zero = Score(0)

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