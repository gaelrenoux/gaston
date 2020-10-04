package fr.renoux.gaston.model

import fr.renoux.gaston.util.CollectionImplicits._
import fr.renoux.gaston.util.TupleImplicits._

/** Basic information about a problem. Not getting into the details of preferences and constraints. */
trait Problem {
  implicit val counts: Counts

  val slotSequences: Seq[Seq[Slot]]
  val slots: Set[Slot]
  val topics: Set[Topic]
  val unassignedTopics: BitMap[Slot, Topic]
  val persons: Set[Person]
  val constraints: Set[Constraint]
  val preferences: Set[Preference]

  lazy val realTopics: Set[Topic] = topics.filterNot(_.virtual)
  lazy val forcedTopics: Set[Topic] = topics.filter(_.forced)

  lazy val slotsList: List[Slot] = slots.toList
  lazy val topicsList: List[Topic] = topics.toList
  lazy val realTopicsList: List[Topic] = realTopics.toList
  lazy val personsList: List[Person] = persons.toList
  lazy val constraintsList: List[Constraint] = constraints.toList
  lazy val preferencesList: List[Preference] = preferences.toList
  lazy val personalPreferencesList: List[Preference.Personal] = preferencesList.collect { case pp: Preference.Personal => pp }
  lazy val personalPreferencesListByPerson: BitMap[Person, List[Preference.Personal]] = personalPreferencesList.groupBy(_.person).toBitMap(Nil)

  private lazy val (
    (recordLevelPreferences: Set[Preference.RecordLevel], slotLevelPreferences: Set[Preference.SlotLevel]),
    globalLevelPreferences: Set[Preference.GlobalLevel]
    ) =
    preferences.collect {
      case s: Preference.RecordLevel => Left(Left(s))
      case s: Preference.SlotLevel => Left(Right(s))
      case s: Preference.GlobalLevel => Right(s)
    }.unzipEither.map1(_.unzipEither)

  lazy val impersonalGlobalLevelPreferences: Set[Preference.GlobalLevel] = globalLevelPreferences.filterNot(_.isInstanceOf[Preference.Personal])
  lazy val impersonalGlobalLevelPreferencesList: List[Preference.GlobalLevel] = impersonalGlobalLevelPreferences.toList
  lazy val impersonalSlotLevelPreferences: Set[Preference.SlotLevel] = slotLevelPreferences.filterNot(_.isInstanceOf[Preference.Personal])
  lazy val impersonalSlotLevelPreferencesList: List[Preference.SlotLevel] = impersonalSlotLevelPreferences.toList
  lazy val impersonalRecordLevelPreferences: Set[Preference.RecordLevel] = recordLevelPreferences.filterNot(_.isInstanceOf[Preference.Personal])
  lazy val impersonalRecordLevelPreferencesList: List[Preference.RecordLevel] = impersonalRecordLevelPreferences.toList

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

  val mandatoryTopicsByPerson: BitMap[Person, Set[Topic]]

  val forbiddenTopicsByPerson: BitMap[Person, Set[Topic]]

  /** For everyone, their personal preferences */
  val preferencesByPerson: BitMap[Person, Set[Preference.Personal]]

  /** For each topic, the topics that cannot be held in the same slot because of some constraints (like the same persons
    * are mandatory). */
  val incompatibleTopicsByTopic: BitMap[Topic, Set[Topic]]

  /** For each slot, the topics that cannot be held in that slot because of some constraints (like some mandatory person
    * is missing). */
  val incompatibleTopicsBySlot: BitMap[Slot, Set[Topic]]

  val simultaneousTopicByTopic: BitMap[Topic, Set[Topic]]

}
