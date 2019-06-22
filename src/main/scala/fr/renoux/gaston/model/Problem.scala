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

  lazy val slotsSeq: Seq[Slot] = slots.toSeq
  lazy val topicsSeq: Seq[Topic] = topics.toSeq
  lazy val personsSeq: Seq[Person] = persons.toSeq
  lazy val constraintsSeq: Seq[Constraint] = constraints.toSeq
  lazy val preferencesSeq: Seq[Preference] = preferences.toSeq

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

  lazy val slotCouplesSeq: Seq[(Slot, Slot)] = slotCouples.toSeq

  val personsCount: Int

  val maxTopicCountPerSlot: Map[Slot, Int]

  /** For each persons, its available slots */
  val slotsPerPerson: Map[Person, Set[Slot]]

  /** For each slot, the available persons */
  val personsPerSlot: Map[Slot, Set[Person]]

  /** For each slot, the number of available persons */
  val personsCountPerSlot: Map[Slot, Int]

  val mandatoryTopicsPerPerson: Map[Person, Set[Topic]]

  val forbiddenTopicsPerPerson: Map[Person, Set[Topic]]

  /** For everyone, their personal preferences */
  val preferencesPerPerson: Map[Person, Set[Preference.Personal]]

  /** For each topic, the topics that cannot be held in the same slot because of some constraints (like the same persons
    * are mandatory). */
  val incompatibleTopicsPerTopic: Map[Topic, Set[Topic]]

  /** For each slot, the topics that cannot be held in that slot because of some constraints (like some mandatory person
    * is missing). */
  val incompatibleTopicsPerSlot: Map[Slot, Set[Topic]]

  val simultaneousTopicPerTopic: Map[Topic, Set[Topic]]

}
