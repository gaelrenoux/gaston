package fr.renoux.gaston.model

import fr.renoux.gaston.util.Identified

/** Something some persons are doing during a slot on the schedule. A roleplaying session, a round table, a class,
  * whatever.
  * @param slots Slots on which the topic must be, None meaning it can be on any slot.
  * @param forced Topic must be on the schedule.
  * @param virtual Topic created for technical reasons, not a real topic. Cannot be moved or removed from its slot.
  **/
final case class Topic(
    id: Int,
    name: String,
    mandatory: Set[Person] = Set.empty,
    forbidden: Set[Person] = Set.empty,
    min: Int = Topic.DefaultMin,
    max: Int = Topic.DefaultMax, // TODO bad max, remove default value (Person.MaxCount would be better)
    slots: Option[Set[Slot]] = None,
    forced: Boolean = false,
    virtual: Boolean = false
) extends Identified {

  /** To facilitate writing schedules */
  def apply(persons: Person*): (Topic, Set[Person]) = this -> persons.toSet

  def toShortString: String = s"$id -> $name"

  def toLongString: String = s"Topic($id, $name, mandatory=${mandatory.map(_.name).mkString("[", ", ", "]")}, " +
    s"forbidden=${forbidden.map(_.name).mkString("[", ", ", "]")}, $min to $max, " +
    s"${slots.fold("")(s => s.map(_.name).mkString("forced = [", ", ", "], "))}" +
    s"forced=$forced, virtual=$virtual"
}

object Topic {

  val DefaultMin = 1

  val DefaultMax = 10
}
