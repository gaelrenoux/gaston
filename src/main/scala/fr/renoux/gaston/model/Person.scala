package fr.renoux.gaston.model

// TODO get the weight out of person and have the person inside the weight object ? It is only used during the scoring in Problem.

/** Someone. */
final case class Person(
    id: Int,
    name: String,
    weight: Weight = Weight.Default
)

object Person {
  val MaxCount: Int = 10 * 1000
}
