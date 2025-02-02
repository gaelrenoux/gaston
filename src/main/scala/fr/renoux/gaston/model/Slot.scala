package fr.renoux.gaston.model

import fr.renoux.gaston.util.Identified


/** A slot in the schedule. A person can only have one topic during a slot. */
final case class Slot(
    id: Int,
    name: String,
    personsPresent: Set[Person],
    next: Option[Slot],
    maxTopics: Int = Int.MaxValue
) extends Identified {

  /** To facilitate writing schedules */
  def apply(records: (Topic, Set[Person])*)(using problem: Problem): Seq[Record] =
    records.map(r => Record(this, r._1, r._2))

  lazy val personsPresentCount: Int = personsPresent.size

  lazy val hasNext: Boolean = next.nonEmpty

  lazy val toShortString: String = s"$id -> $name"

  lazy val toLongString: String =
    s"Slot($id, $name,${next.fold("")(s => s" next: ${s.name},")} ${personsPresent.map(_.name).mkString("(", ",", ")")}${if (maxTopics < Int.MaxValue) s", max: $maxTopics" else ""})"

  lazy val toAbstract: (Int, List[Person.Id], Option[Int], Int) = (id, personsPresent.map(_.id).toList.sorted, next.map(_.id), maxTopics)

  override def hashCode(): Int = id
}

//TODO call to hashcode is a minor (9%) hot-spot ! Same for topic and person !
