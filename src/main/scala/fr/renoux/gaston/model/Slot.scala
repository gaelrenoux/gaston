package fr.renoux.gaston.model

import fr.renoux.gaston.util.Identified


/** A slot in the schedule. A person can only have one topic during a slot. */
final case class Slot(
    id: Int,
    name: String,
    personsPresent: Set[Person],
    maxTopics: Int = Int.MaxValue
) extends Identified {

  /** To facilitate writing schedules */
  def apply(records: (Topic, Set[Person])*)(implicit problem: Problem): Seq[Record] =
    records.map(r => Record(this, r._1, r._2))

  lazy val personsPresentCount: Int = personsPresent.size
}

//TODO call to hashcode is a minor (9%) hot-spot ! Same for topic and person !
