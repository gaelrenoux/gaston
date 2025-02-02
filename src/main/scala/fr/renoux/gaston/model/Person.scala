package fr.renoux.gaston.model

import fr.renoux.gaston.util.Identified

// TODO get the weight out of person and have the person inside the weight object ? It is only used during the scoring in Problem.

/** Someone.
 * @param baseScore Always added to this person by default, independently of their preferences.
 */
final case class Person(
    id: Person.Id,
    name: String,
    weight: Weight = Weight.Default,
    baseScore: Score = Score.Zero
) extends Identified {

  def toShortString: String = s"$id -> $name"

  def toLongString: String = s"Person($id, $name, ${weight.value}${if (baseScore == Score.Zero) "" else s", baseScore=$baseScore"})"

  lazy val toAbstract: (Person.Id, Double, Double) = (id, weight.value, baseScore.value)

  override def hashCode(): Int = id
}

object Person {
  type Id = Int
  val MaxCount: Int = 10 * 1000
}
