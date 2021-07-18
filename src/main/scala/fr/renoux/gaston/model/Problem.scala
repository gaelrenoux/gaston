package fr.renoux.gaston.model

import fr.renoux.gaston.util.BitMap.syntax._
import fr.renoux.gaston.util.ArraySet.syntax._
import fr.renoux.gaston.util.{ArraySet, BitMap, Count}

/** Basic information about a problem. Not getting into the details of preferences and constraints. */
trait Problem {
  val slotSequences: Array[Array[Slot]]
  val slots: Array[Slot] = slotSequences.flatten
  val topics: Array[Topic]
  val unassignedTopics: BitMap[Slot, Topic]
  val persons: Array[Person]
  val constraints: Array[Constraint]
  val preferences: Array[Preference]

  implicit val slotsCount: Count[Slot] = Count[Slot](slots.length)
  implicit val topicsCount: Count[Topic] = Count[Topic](topics.length)
  implicit val personsCount: Count[Person] = Count[Person](persons.length)
  lazy val counts: Counts = Counts(slots = slotsCount.value, topics = topicsCount.value, persons = personsCount.value)

  lazy val weightsByPersonId: Array[Weight] = persons.sortBy(_.id).map(_.weight)

  lazy val realTopics: Array[Topic] = topics.filterNot(_.virtual)
  lazy val realTopicsSet: ArraySet[Topic] = realTopics.toArraySet
  lazy val forcedTopics: Array[Topic] = topics.filter(_.forced)

  lazy val personalPreferencesList: Array[Preference.Personal] = preferences.collect { case pp: Preference.Personal => pp }
  lazy val personalPreferencesListByPerson: BitMap[Person, Array[Preference.Personal]] = personalPreferencesList.groupBy(_.person).toBitMap()

  private lazy val recordLevelPreferences = preferences.collect { case p: Preference.RecordLevel => p }
  private lazy val slotLevelPreferences = preferences.collect { case p: Preference.SlotLevel => p }
  private lazy val globalLevelPreferences = preferences.collect { case p: Preference.GlobalLevel => p }

  lazy val impersonalGlobalLevelPreferences: Array[Preference.GlobalLevel] = globalLevelPreferences.filterNot(_.isInstanceOf[Preference.Personal])
  lazy val impersonalSlotLevelPreferences: Array[Preference.SlotLevel] = slotLevelPreferences.filterNot(_.isInstanceOf[Preference.Personal])
  lazy val impersonalRecordLevelPreferences: Array[Preference.RecordLevel] = recordLevelPreferences.filterNot(_.isInstanceOf[Preference.Personal])


  lazy val slotLevelConstraints: Array[Constraint.SlotLevel] = constraints.collect { case c: Constraint.SlotLevel => c }
  lazy val globalLevelConstraints: Array[Constraint.GlobalLevel] = constraints.collect { case c: Constraint.GlobalLevel => c }

  /** All slot unordered couples, without duplicates. The first element of the couple is always lower than the second. */
  lazy val slotCouples: Array[(Slot, Slot)] = for {
    s1 <- slots
    s2 <- slots if s1.name < s2.name
  } yield (s1, s2)

  val mandatoryTopicsByPerson: BitMap[Person, ArraySet[Topic]]

  val forbiddenTopicsByPerson: BitMap[Person, ArraySet[Topic]]

  /** For each topic, the topics that cannot be held in the same slot because of some constraints (like the same persons
    * are mandatory). */
  val incompatibleTopicsByTopic: BitMap[Topic, ArraySet[Topic]]

  /** For each slot, the topics that cannot be held in that slot because of some constraints (like some mandatory person
    * is missing). */
  val incompatibleTopicsBySlot: BitMap[Slot, ArraySet[Topic]]

  /** For each topic, all other topics that need to happen simultaneously. */
  val simultaneousTopicByTopic: BitMap[Topic, ArraySet[Topic]]

  /** For each topic, all linked topics including itself. Same as simultaneousTopicByTopic but the key topic is also in the array. */
  val linkedTopicsByTopic: BitMap[Topic, ArraySet[Topic]]

}
