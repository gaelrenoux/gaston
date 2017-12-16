package fr.renoux.gaston.model

case class Weight(value: Double) extends AnyVal {
  def *(s: Score) = Score(value * s.value)
}

object Weight {

  val Default = Weight(1.0)

  def combine(w: Weight, ws: Weight*): Weight = Weight(
    ws.map(_.value).foldLeft(w.value)(_ * _)
  )

  implicit object WeightINumeric extends Numeric[Weight] {
    override def plus(x: Weight, y: Weight): Weight = Weight(x.value + y.value)

    override def minus(x: Weight, y: Weight): Weight = Weight(x.value - y.value)

    override def times(x: Weight, y: Weight): Weight = Weight(x.value * y.value)

    override def negate(x: Weight): Weight = Weight(-x.value)

    override def fromInt(x: Int): Weight = Weight(x)

    override def toInt(x: Weight): Int = x.value.toInt

    override def toLong(x: Weight): Long = x.value.toLong

    override def toFloat(x: Weight): Float = x.value.toFloat

    override def toDouble(x: Weight): Double = x.value.toDouble

    override def compare(x: Weight, y: Weight): Int = x.value.compareTo(y.value)
  }

}