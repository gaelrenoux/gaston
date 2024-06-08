package fr.renoux.gaston.model

import fr.renoux.gaston.util.Identified

/** Something some persons are doing during a slot on the schedule. A roleplaying session, a round table, a class,
  * whatever.
  * @param slots Slots on which the topic must be, None meaning it can be on any slot.
  * @param isFollowup true Only if this topic is another's topic followup
  * @param forced Topic must be on the schedule.
  */
final case class Topic(
    id: Int,
    name: String,
    mandatory: Set[Person] = Set.empty,
    forbidden: Set[Person] = Set.empty,
    min: Int = Topic.DefaultMin,
    max: Int = Topic.DefaultMax, // TODO bad max, remove default value (Person.MaxCount would be better)
    slots: Option[Set[Slot]] = None,
    followup: Option[Topic] = None,
    isFollowup: Boolean = false,
    forced: Boolean = false,
) extends Identified {

  /** true if this topic is only schedulable on a single slot. */
  lazy val requiresSingleSpecificSlot: Boolean = slots.exists(_.size == 1)

  /** To facilitate writing schedules */
  def apply(persons: Person*): (Topic, Set[Person]) = this -> persons.toSet

  val isSynthetic: Boolean = name.startsWith(Topic.SyntheticPrefix)

  val toShortString: String = s"$id -> $name"

  val toLongString: String = s"Topic($id, $name, mandatory=${mandatory.map(_.name).mkString("[", ", ", "]")}, " +
    s"forbidden=${forbidden.map(_.name).mkString("[", ", ", "]")}, $min to $max, " +
    slots.fold("")(s => s.map(_.name).mkString("forced = [", ", ", "], ")) +
    followup.fold("")(t => s"followup = ${t.toShortString}, ") +
    (if (isFollowup) "isFollowup = true, " else "") +
    s"forced=$forced" +
    ")"
}

object Topic {

  /** A prefix on the topic-name for synthetic topics (topics created by Gaston, not present in the input). */
  val SyntheticPrefix = "@"

  val DefaultMin = 1

  val DefaultMax = 10
}
