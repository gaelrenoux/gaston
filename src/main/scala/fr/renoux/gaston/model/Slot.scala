package fr.renoux.gaston.model

/** A slot in the schedule. A person can only have one topic during a slot. */
case class Slot(
    name: String,
    personsPresent: Set[Person],
    maxTopics: Int = Int.MaxValue
) {

  /** To facilitate writing schedules */
  def apply(records: (Topic, Set[Person])*): Seq[Record] =
    records.map(r => Record(this, r._1, r._2))

  lazy val personsPresentCount: Int = personsPresent.size
}
