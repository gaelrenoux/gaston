package fr.renoux.gaston.model

/** Something some persons are doing during a slot on the schedule. A roleplaying session, a round table, a class,
  * whatever.
  * @param slots Slots on which the topic must be, None meaning it can be on any slot.
  * @param forced Topic must be on the schedule.
  * @param virtual Topic created for technical reasons, not a real topic. Cannot be moved or removed from its slot.
  * */
final case class Topic(
    name: String,
    mandatory: Set[Person] = Set.empty,
    forbidden: Set[Person] = Set.empty,
    min: Int = Topic.DefaultMin,
    max: Int = Topic.DefaultMax,
    slots: Option[Set[Slot]] = None,
    forced: Boolean = false,
    virtual: Boolean = false
) {

  /** Duplicate this Topic as several occurrences */
  def occurrences(count: Int): Seq[Topic] = (1 to count).map(i => copy(name = s"$name ${Topic.OccurrenceMarker}$i"))

  /** Duplicate this Topic as multiple */
  def multiple(count: Int): Seq[Topic] = (1 to count).map(i => copy(name = s"$name ${Topic.MultipleMarker}$i"))

  /** To facilitate writing schedules */
  def apply(persons: Person*): (Topic, Set[Person]) = this -> persons.toSet
}

object Topic {

  val DefaultMin = 1

  val DefaultMax = 10

  val OccurrenceMarker = "#"

  val MultipleMarker = "~"

  /** Topics for people assigned to doing nothing. */
  def nothing(slot: Slot, min: Int, max: Int): Topic = Topic(s"Nothing (${slot.name})", min = min, max = max, slots = Some(Set(slot)), virtual = true)

  /** Topics for people not assigned yet on some slot. */
  def unassigned(slot: Slot): Topic = Topic(s"[${slot.name}]", max = Person.MaxCount, slots = Some(Set(slot)), virtual = true)
}
