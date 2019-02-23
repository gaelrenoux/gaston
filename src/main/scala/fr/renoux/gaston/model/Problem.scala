package fr.renoux.gaston.model

import fr.renoux.gaston.util.CollectionImplicits._

/** Basic information about a problem. Not getting into the details of preferences and constraints. */
trait Problem {
  val parallelization: Int
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

  val personsCount: Int

  val personsPerSlot: Map[Slot, Set[Person]]

  val personsCountPerSlot: Map[Slot, Int]

  val mandatoryPersonsPerTopic: Map[Topic, Set[Person]]

  val forbiddenPersonsPerTopic: Map[Topic, Set[Person]]

  val minNumberPerTopic: Map[Topic, Int]

  val maxNumberPerTopic: Map[Topic, Int]

  val mandatoryTopicsPerPerson: Map[Person, Set[Topic]]

}
