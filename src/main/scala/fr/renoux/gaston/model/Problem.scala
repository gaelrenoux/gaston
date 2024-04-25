package fr.renoux.gaston.model

import fr.renoux.gaston.util.BitMap
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

  lazy val realTopics: Set[Topic] = topics.filterNot(t => t.virtual || t.isFollowup)
  lazy val forcedTopics: Set[Topic] = topics.filter(_.forced)
  /** All forced topics, starting with the ones that are limited to the least number of possible slots, up to the ones that can be on any slot. */
  lazy val forcedTopicsMostToLeastConstrained: Seq[Topic] =
    forcedTopics.toSeq.view.map { topic => topic -> topic.slots.fold(slots.size)(_.size) }.sortBy(_._2).map(_._1).toSeq
  lazy val topicsWithFollowups: Set[(Topic, Topic)] = topics.flatMap { t => t.followup.map(t -> _) }

  lazy val slotsList: List[Slot] = slots.toList
  lazy val slotsToNextSlot: Map[Slot, Slot] = slots.flatMap(s => s.next.map(s -> _)).toMap
  lazy val slotsToPreviousSlot: Map[Slot, Slot] = slotsToNextSlot.map(_.swap)

  lazy val topicsList: List[Topic] = topics.toList
  lazy val realTopicsList: List[Topic] = realTopics.toList
  lazy val personsList: List[Person] = persons.toList
  lazy val constraintsList: List[Constraint] = constraints.toList
  lazy val preferencesList: List[Preference] = preferences.toList
  lazy val personalPreferencesList: List[Preference.Personal] = preferencesList.collect { case pp: Preference.Personal => pp }
  lazy val personalPreferencesListByPerson: BitMap[Person, List[Preference.Personal]] = personalPreferencesList.groupBy(_.person).toBitMap(Nil)

  lazy val baseScoreByPerson: Map[Person, Score] =
    if (persons.exists(_.baseScore != Score.Zero)) persons.map(p => p -> p.baseScore).toMap
    else Map.empty

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

  lazy val hasGlobalPreferencesWherePersonsMatter: Boolean = globalLevelPreferences.exists(_.personsMatter)

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

  def toFormattedString: String

}
