package fr.renoux.gaston.model

import fr.renoux.gaston.util.{Identified, NumberUtils}

/** Something some persons are doing during a slot on the schedule. A roleplaying session, a round table, a class,
  * whatever.
  * @param slots Slots on which the topic must be, None meaning it can be on any slot.
  * @param isFollowup true Only if this topic is another's topic followup
  * @param forced Topic must be on the schedule.
  * @param isUnassigned Special flag on synthetic topics that actually stores unassigned persons.
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
    isUnassigned: Boolean = false
) extends Identified {

  /** true if this topic is only schedulable on a single slot. */
  lazy val requiresSingleSpecificSlot: Boolean = slots.exists(_.size == 1)

  /** To facilitate writing schedules */
  def apply(persons: Person*): (Topic, Set[Person]) = this -> persons.toSet

  val toShortString: String = s"$id -> $name"

  val toLongString: String = s"Topic($id, $name, mandatory=${mandatory.map(_.name).mkString("[", ", ", "]")}, " +
    s"forbidden=${forbidden.map(_.name).mkString("[", ", ", "]")}, $min to $max, " +
    slots.fold("")(s => s.map(_.name).mkString("forced = [", ", ", "], ")) +
    followup.fold("")(t => s"followup = ${t.toShortString}, ") +
    (if (isFollowup) "isFollowup = true, " else "") +
    s"forced=$forced" +
    ")"

  lazy val toAbstract: (Int, Set[Person.Id], Set[Person.Id], Int, Int, Option[Set[Int]], Option[Int], Boolean, Boolean, Boolean) = (
    id,
    mandatory.map(_.id), forbidden.map(_.id),
    min, max,
    slots.map(_.map(_.id)),
    followup.map(_.id),
    isFollowup, forced, isUnassigned
  )
}

object Topic {

  /** A prefix on the topic-name for synthetic topics (topics created by Gaston, not present in the input). Currently
    * only unassigned topics, but reserving it just in case. */
  val SyntheticPrefix = "@"

  def unassignedName(slotName: String): String = s"${Topic.SyntheticPrefix}Unassigned ($slotName)"

  /** Create a special "unassigned" topic */
  // TODO maybe find a better name than 'unassigned', maybe 'pending' ?
  def unassigned(id: Int, slot: Slot, min: Int = 0, max: Int = NumberUtils.IntLowMaxValue.value): Topic = {
    val realMin = if (min <= 1) 0 else min
    Topic(id, unassignedName(slot.name), min = realMin, max = max, slots = Some(Set(slot)), forced = min == 0, isUnassigned = true)
  }

  val DefaultMin = 1

  val DefaultMax = 10
}
