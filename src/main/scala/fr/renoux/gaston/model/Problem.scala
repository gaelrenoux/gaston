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

  lazy val realTopics: Set[Topic] = topics.filterNot(_.virtual)

  lazy val slotsList: List[Slot] = slots.toList
  lazy val topicsList: List[Topic] = topics.toList
  lazy val realTopicsList: List[Topic] = realTopics.toList
  lazy val personsList: List[Person] = persons.toList
  lazy val constraintsList: List[Constraint] = constraints.toList
  lazy val preferencesList: List[Preference] = preferences.toList
  lazy val personalPreferencesList: List[Preference.Personal] = preferencesList.collect { case pp: Preference.Personal => pp }
  lazy val personalPreferencesListPerPerson: Map[Person, List[Preference.Personal]] = personalPreferencesList.groupBy(_.person).withDefaultValue(Nil)
  lazy val impersonalPreferences: Set[Preference] = preferences.filterNot(_.isInstanceOf[Preference.Personal])
  lazy val impersonalPreferencesList: List[Preference] = preferencesList.filterNot(_.isInstanceOf[Preference.Personal])

  lazy val (slotLevelPreferences: Set[Preference.SlotLevel], globalLevelPreferences: Set[Preference]) =
    preferences.collect {
      case s: Preference.SlotLevel => Left(s)
      case s: Preference => Right(s)
    }.unzipEither

  lazy val impersonalSlotLevelPreferences: Set[Preference.SlotLevel] = slotLevelPreferences.filterNot(_.isInstanceOf[Preference.Personal])
  lazy val impersonalSlotLevelPreferencesList: List[Preference.SlotLevel] = impersonalSlotLevelPreferences.toList
  lazy val impersonalGlobalLevelPreferences: Set[Preference] = globalLevelPreferences.filterNot(_.isInstanceOf[Preference.Personal])
  lazy val impersonalGlobalLevelPreferencesList: List[Preference] = impersonalGlobalLevelPreferences.toList

  lazy val (slotLevelConstraints: Set[Constraint.SlotLevel], globalLevelConstraints: Set[Constraint]) =
    constraints.collect {
      case s: Constraint.SlotLevel => Left(s)
      case s: Constraint => Right(s)
    }.unzipEither

  /** All slot unordered couples, without duplicates. The first element of the couple is always lower than the second. */
  lazy val slotCouples: Set[(Slot, Slot)] = for {
    s1 <- slots
    s2 <- slots if s1.name < s2.name
  } yield (s1, s2)

  lazy val slotCouplesSeq: Seq[(Slot, Slot)] = slotCouples.toSeq

  val personsCount: Int

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
