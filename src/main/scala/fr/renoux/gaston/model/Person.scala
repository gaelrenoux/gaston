package fr.renoux.gaston.model

// TODO get the weight out of person and have the person inside the weight object ? It is only used during the scoring in Problem.

/** Someone. */
final case class Person(
    id: Person.Id,
    name: String,
    weight: Weight = Weight.Default
) extends Identified

object Person {
  type Id = Int
  val MaxCount: Int = 10 * 1000
}
