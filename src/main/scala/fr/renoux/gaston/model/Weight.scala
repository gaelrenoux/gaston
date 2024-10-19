package fr.renoux.gaston.model

/** The higher the weight of a person, the more its preferences matter when calculating a score for a set of preferences
  * involving multiple persons. */
// TODO enforce that the value is strictly positive
final case class Weight(value: Double) extends AnyVal {
  @inline def *(s: Score): Score = Score(value * s.value)
}

object Weight extends (Double => Weight) {

  val Default: Weight = Weight(1.0)

  /** Multiply all arguments weights (at least one) together */
  def combine(w: Weight, ws: Weight*): Weight = Weight(
    ws.map(_.value).foldLeft(w.value)(_ * _)
  )

  /** Implementation of the Fractional typeclass for Weight */
  implicit object WeightIsFractional extends Fractional[Weight] {
    override def plus(x: Weight, y: Weight): Weight = Weight(x.value + y.value)

    override def minus(x: Weight, y: Weight): Weight = Weight(x.value - y.value)

    override def times(x: Weight, y: Weight): Weight = Weight(x.value * y.value)

    override def div(x: Weight, y: Weight): Weight = Weight(x.value / y.value)

    override def negate(x: Weight): Weight = Weight(-x.value)

    override def fromInt(x: Int): Weight = Weight(x.toDouble)

    override def toInt(x: Weight): Int = x.value.toInt

    override def toLong(x: Weight): Long = x.value.toLong

    override def toFloat(x: Weight): Float = x.value.toFloat

    override def toDouble(x: Weight): Double = x.value

    override def compare(x: Weight, y: Weight): Int = x.value.compareTo(y.value)

    override def abs(x: Weight): Weight = Weight(math.abs(x.value))

    override def parseString(str: String): Option[Weight] = str.toDoubleOption.map(Weight)
  }

}
