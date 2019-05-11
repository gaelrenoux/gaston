package fr.renoux.gaston.model

import fr.renoux.gaston.util.CollectionImplicits._

/** Basic information about a problem. Not getting into the details of preferences and constraints. */
trait Problem {
  val slotSequences: Seq[Seq[Slot]]
  val slots: Set[Slot]
  val topics: Set[Topic]
  val persons: Set[Person]
  val constraints: Set[Constraint]
  val preferences: Set[Preference]

  lazy val (slotLevelPreferences: Set[Preference.SlotLevel], globalLevelPreferences: Set[Preference]) =
    preferences.collect {
      case s: Preference.SlotLevel => Left(s)
      case s => Right(s)
    }.unzipEither

  lazy val (slotLevelConstraints: Set[Constraint.SlotLevel], globalLevelConstraints: Set[Constraint]) =
    constraints.collect {
      case s: Constraint.SlotLevel => Left(s)
      case s => Right(s)
    }.unzipEither

  /** All slot unordered couples, without duplicates. The first element of the couple is always lower than the second. */
  lazy val slotCouples: Set[(Slot, Slot)] = for {
    s1 <- slots
    s2 <- slots if s1.name < s2.name
  } yield (s1, s2)

  val personsCount: Int

  val maxTopicCountPerSlot: Map[Slot, Int]

  val personsPerSlot: Map[Slot, Set[Person]]

  val personsCountPerSlot: Map[Slot, Int]

  val mandatoryTopicsPerPerson: Map[Person, Set[Topic]]

  /** For each topic, the topics that cannot be held in the same slot because of some constraints (like the same persons
    * are mandatory). */
  val incompatibleTopicsPerTopic: Map[Topic, Set[Topic]]

  /** For each slot, the topics that cannot be held in that slot because of some constraints (like some mandatory person
    * is missing). */
  val incompatibleTopicsPerSlot: Map[Slot, Set[Topic]]

}
