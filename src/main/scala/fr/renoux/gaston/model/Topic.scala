package fr.renoux.gaston.model

/** Something some persons are doing during a slot on the schedule. A roleplaying session, a round table, a class,
  * whatever.
  * @param slots Slots on which the topic must be, None meaning it can be on any slot.
  * @param forced Topic must be on the schedule.
  * @param virtual Topic created for technical reasons, not a real topic. Cannot be moved or removed from its slot.
  **/
final case class Topic(
    name: String,
    mandatory: Set[Person] = Set.empty,
    forbidden: Set[Person] = Set.empty,
    min: Int = Topic.DefaultMin,
    max: Int = Topic.DefaultMax, // TODO bad max, remove default value (Person.MaxCount would be better)
    slots: Option[Set[Slot]] = None,
    forced: Boolean = false,
    virtual: Boolean = false
) {

  /** To facilitate writing schedules */
  def apply(persons: Person*): (Topic, Set[Person]) = this -> persons.toSet
}

object Topic {

  val DefaultMin = 1

  val DefaultMax = 10
}
