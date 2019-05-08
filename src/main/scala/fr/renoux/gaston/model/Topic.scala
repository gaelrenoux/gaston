package fr.renoux.gaston.model

/** Something some persons are doing during a slot on the schedule. A roleplaying session, a round table, a class,
  * whatever. */
case class Topic(name: String) extends AnyVal {

  /** To facilitate writing schedules */
  def apply(persons: Person*): (Topic, Set[Person]) = this -> persons.toSet
}

object Topic {

  def nothing(slot: Slot) = Topic(s"Nothing (${slot.name})")

}