package fr.renoux.gaston.model

import fr.renoux.gaston.model.constraints.{TopicsNotSimultaneous, TopicsSimultaneous}
import fr.renoux.gaston.util.*
import fr.renoux.gaston.util.CanGroupToMap.given

/** Basic information about a problem. Not getting into the details of preferences and constraints. */
final case class Problem(
    slotSequences: Seq[Seq[Slot]],
    topicsSet: Set[Topic],
    unassignedTopics: ArrayMap[Slot, Topic],
    personsSet: Set[Person],
    constraints: Set[Constraint],
    preferences: Set[Preference]
)(implicit val counts: Counts) {

  /* SLOTS */

  val slotsSet: Set[Slot] = slotSequences.flatten.toSet
  lazy val slotsList: List[Slot] = slotsSet.toList.sortBy(_.id)
  lazy val slotsToNextSlot: Map[Slot, Slot] = slotsSet.flatMap(s => s.next.map(s -> _)).toMap
  lazy val slotsToPreviousSlot: Map[Slot, Slot] = slotsToNextSlot.map(_.swap)

  /** All slot unordered couples, without duplicates. The first element of the couple is always lower than the second (by lexicographic order). */
  lazy val slotCouplesList: List[(Slot, Slot)] = for {
    s1 <- slotsList
    s2 <- slotsList if s1.name < s2.name
  } yield (s1, s2)



  /* TOPICS */

  lazy val topicsList: List[Topic] = topicsSet.toList.sortBy(_.id)
  lazy val forcedTopics: Set[Topic] = topicsSet.filter(_.forced)
  /** All forced topics, starting with the ones that are limited to the least number of possible slots, up to the ones that can be on any slot. */
  lazy val forcedTopicsMostToLeastConstrained: Seq[Topic] =
    forcedTopics.toSeq.view.map { topic => topic -> topic.slots.fold(slotsSet.size)(_.size) }.sortBy(_._2).map(_._1).toSeq
  lazy val topicsWithFollowups: Set[(Topic, Topic)] = topicsSet.flatMap { t => t.followup.map(t -> _) }



  /* PERSONS */

  lazy val personsList: List[Person] = personsSet.toList.sortBy(_.id)
  lazy val personsCount: Int = personsSet.size
  lazy val baseScoreByPerson: Map[Person, Score] =
    if (personsSet.exists(_.baseScore != Score.Zero)) personsSet.map(p => p -> p.baseScore).toMap
    else Map.empty



  /* PREFERENCES */

  lazy val preferencesList: List[Preference] = preferences.toList
  lazy val personalPreferencesList: List[Preference.Personal] = preferencesList.collect { case pp: Preference.Personal => pp }
  lazy val personalPreferencesListByPerson: ArrayMap[Person, List[Preference.Personal]] = personalPreferencesList.groupBy(_.person).toArrayMap(Nil)


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

  /** For everyone, their personal preferences */
  lazy val preferencesByPerson: ArrayMap[Person, Set[Preference.Personal]] = preferences.collect {
    case p: Preference.Personal => p
  }.groupBy(_.person).toArrayMap(Set.empty)



  /* CONSTRAINT */

  lazy val constraintsList: List[Constraint] = constraints.toList
  lazy val (slotLevelConstraints: Set[Constraint.SlotLevel], globalLevelConstraints: Set[Constraint]) =
    constraints.collect {
      case s: Constraint.SlotLevel => Left(s)
      case s: Constraint => Right(s)
    }.unzipEither

  lazy val mandatoryTopicsByPerson: ArrayMap[Person, Set[Topic]] =
    topicsSet.flatMap(t => t.mandatory.map(_ -> t)).groupToMap.toArrayMap(Set.empty)

  lazy val forbiddenTopicsByPerson: ArrayMap[Person, Set[Topic]] =
    topicsSet.flatMap(t => t.forbidden.map(_ -> t)).groupToMap.toArrayMap(Set.empty)

  /** For each topic, the topics that cannot be held in the same slot because of some constraints (like the same persons
   * are mandatory). */
  lazy val incompatibleTopicsByTopic: ArrayMap[Topic, Set[Topic]] = {
    val conflictingMandatories = for {
      topic1 <- topicsSet
      topic2 <- topicsSet
      if topic1.mandatory.intersect(topic2.mandatory).nonEmpty
    } yield (topic1, topic2)

    val notSimultaneous = constraints.collect {
      case TopicsNotSimultaneous(ts) =>
        val topics = this.topicsSet.filter(ts.contains)
        topics.cross(topics)
    }.flatten

    (conflictingMandatories ++ notSimultaneous).groupToMap.toArrayMap(Set.empty)
  }

  /** For each slot, the topics that cannot be held in that slot because of some constraints (like some mandatory person
   * is missing). */
  lazy val incompatibleTopicsBySlot: ArrayMap[Slot, Set[Topic]] = {
    val couples = for {
      slot <- slotsSet
      topic <- topicsSet
      if topic.mandatory.exists(!slot.personsPresent.contains(_)) || topic.slots.exists(!_.contains(slot))
    } yield (slot, topic)
    couples.groupToMap.toArrayMap(Set.empty)
  }

  lazy val simultaneousTopicByTopic: ArrayMap[Topic, Set[Topic]] = {
    constraints.collect {
      case TopicsSimultaneous(ts) => ts.map(t => t -> (ts - t))
    }.flatten.toMap.toArrayMap(Set.empty)
  }


  /* FORMATTING */

  lazy val toFormattedString: String = {
    val builder = new StringBuilder("Problem:\n")
    builder.append("  Slots:\n")
    slotsList.sortBy(_.name).map(_.toLongString).foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Topics:\n")
    topicsList.sortBy(_.name).map(_.toLongString).foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Persons:\n")
    personsList.sortBy(_.name).map(_.toLongString).foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Constraints:\n")
    constraintsList.map(_.toLongString).sorted.foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Preferences:\n")
    preferencesList.map(_.toLongString).sorted.foreach(builder.append("    ").append(_).append("\n"))
    builder.toString
  }

  lazy val toAbstract: Product = {
    val abstractSlots = slotSequences.mapMap(_.toAbstract).map(_.sortBy(_._1))
    val abstractTopics = topicsSet.map(_.toAbstract).toSeq.sortBy(_._1)
    val abstractPersons = personsSet.map(_.toAbstract).toSeq.sortBy(_._1)
    val abstractConstraints = constraints.map(_.toAbstract)
    val abstractPreferences = preferences.map(_.toAbstract)
    (abstractSlots, abstractTopics, abstractPersons, abstractConstraints, abstractPreferences)
  }

}

object Problem {
  val Empty: Problem = new Problem(Seq.empty, Set.empty, ArrayMap.empty, Set.empty, Set.empty, Set.empty)(Counts.Empty)
}
