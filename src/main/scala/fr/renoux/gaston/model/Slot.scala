package fr.renoux.gaston.model

/** A slot in the schedule. A person can only have one topic during a slot. */
case class Slot(name: String) extends AnyVal {

  /** To facilitate writing schedules */
  def apply(records: (Topic, Set[Person])*): Seq[Schedule.Record] =
    records.map(r => Schedule.Record(this, r._1, r._2))
}
