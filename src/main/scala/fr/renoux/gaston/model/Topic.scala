package fr.renoux.gaston.model

import fr.renoux.gaston.input.InputSettings

/** Something some persons are doing during a slot on the schedule. A roleplaying session, a round table, a class,
  * whatever.
  * @param movable Can the topic be moved to another slot than the one it is on ?
  * @param removable Can the topic be removed from the schedule ?
  * */
case class Topic(
    name: String,
    mandatory: Set[Person] = Set(),
    forbidden: Set[Person] = Set(),
    min: Int = Topic.DefaultMin,
    max: Int = Topic.DefaultMax,
    movable: Boolean = true,
    removable: Boolean = true
) {

  /** Duplicate this Topic as several occurrences */
  def occurrences(count: Int): Seq[Topic] = (1 to count).map(i => copy(name = s"$name ${Topic.OccurrenceMarker}$i"))

  /** Duplicate this Topic as multiple */
  def multiple(count: Int): Seq[Topic] = (1 to count).map(i => copy(name = s"$name ${Topic.MultipleMarker}$i"))

  /** To facilitate writing schedules */
  def apply(persons: Person*): (Topic, Set[Person]) = this -> persons.toSet
}

object Topic {

  private val DefaultMin = InputSettings.defaultDefaultMinPersonsPerTopic

  private val DefaultMax = InputSettings.defaultDefaultMaxPersonsPerTopic

  val OccurrenceMarker = "#"

  val MultipleMarker = "~"

  /** Topics for people assigned to doing nothing. */
  def nothing(slot: Slot) = Topic(s"Nothing (${slot.name})", movable = false)

  /** Topics for people not assigned yet on some slot. */
  def unassigned(slot: Slot) = Topic(s"[${slot.name}]", movable = false, removable = false)
}