package fr.renoux.gaston.model

import cats.implicits.*
import fr.renoux.gaston.util.*

import scala.annotation.tailrec

/** A schedule for a specific slot */
final case class SlotSchedule(
    slot: Slot,
    private val wrapped: Map[Topic, Record]
)(using val problem: Problem) {

  @inline private def updateWrapped(w: Map[Topic, Record]): SlotSchedule =
    copy(wrapped = w)

  @inline def updateTopicRecord(topic: Topic)(f: Record => Record): SlotSchedule = {
    val updated = wrapped.get(topic).map(f).fold(wrapped)(wrapped.updated(topic, _))
    copy(wrapped = updated)
  }

  lazy val isEmpty: Boolean = records.isEmpty

  lazy val records: Iterable[Record] = wrapped.values
  lazy val recordsSet: Set[Record] = records.toSet
  lazy val recordsList: List[Record] = records.toList
  lazy val recordsThatCanRemovePersons: Iterable[Record] = records.filter(_.canRemovePersons)
  lazy val recordsThatCanAddPersons: Iterable[Record] = records.filter(_.canAddPersons)

  lazy val topics: Iterable[Topic] = wrapped.keys
  lazy val topicsSet: Set[Topic] = wrapped.keySet
  lazy val topicsList: List[Topic] = topics.toList

  /** Topics that can be moved to another slot. Can't move followup directly (move the base topic instead), and can't
   * move if it's restricted to this very slot. */
  lazy val movableTopics: Iterable[Topic] = topics.filterNot(t => t.isFollowup || t.requiresSingleSpecificSlot)
  lazy val movableTopicsSet: Set[Topic] = movableTopics.toSet

  /** Topics that can be removed from the schedule. Can't remove followup directly (remove the base topic instead), and
   * can't remove forced topics. */
  lazy val removableTopics: Iterable[Topic] = topics.filterNot(t => t.forced || t.isFollowup)

  lazy val persons: Iterable[Person] = wrapped.values.flatMap(_.persons)

  /** Topics that cannot be added on this slot, because of the slot itself */
  lazy val permanentlyIncompatibleTopics: Set[Topic] = problem.incompatibleTopicsBySlot(slot)

  /** Topics that cannot be added on this slot as it now (but may be added later if the configuration of the slot changes) */
  lazy val currentlyIncompatibleTopics: Set[Topic] = topicsSet.flatMap(problem.incompatibleTopicsByTopic.apply)

  /** Topics that cannot be added on this slot, because of the slot or other topics */
  lazy val incompatibleTopics: Set[Topic] = permanentlyIncompatibleTopics ++ currentlyIncompatibleTopics

  lazy val countTopics: Int = topics.size
  lazy val maxTopicsLeft: Int = slot.maxTopics - countTopics

  lazy val minPersons: Option[Int] = if (isEmpty) None else Some(topics.foldLeft(0)(_ + _.min))
  lazy val maxPersons: Option[Int] = if (isEmpty) None else Some(topics.foldLeft(0)(_ + _.max))

  lazy val scheduledPersons: Set[Person] = recordsSet.flatMap(_.persons)
  lazy val unscheduledPersons: Set[Person] = slot.personsPresent -- scheduledPersons
  lazy val unscheduledPersonsList: List[Person] = unscheduledPersons.toList
  lazy val personsByTopic: Map[Topic, Set[Person]] = wrapped.mapValuesStrict(_.persons)
  lazy val countPersonsByTopic: Map[Topic, Int] = personsByTopic.mapValuesStrict(_.size)
  lazy val personGroups: Iterable[Set[Person]] = records.view.map(_.persons).toList // not a Set: we do not want to deduplicate identical groups!
  lazy val mandatory: Set[Person] = topicsSet.flatMap(_.mandatory)

  /** For each person, their topic */
  lazy val topicByPerson: Map[Person, Topic] = wrapped.flatMap { case (topic, record) => record.persons.map(p => p -> topic) }

  /** Set of persons unassigned on this slot-schedule * */
  lazy val unassignedPersons: Set[Person] = wrapped.find(_._1.isUnassigned).fold(Set.empty)(_._2.persons)

  lazy val isMinPersonsTooHigh: Boolean = minPersons.exists(_ > problem.personsCount)
  lazy val isMaxPersonsTooLow: Boolean = maxPersons.exists(_ < problem.personsCount)

  /** Clear all non-mandatory persons. Returned schedule is unfilled, obviously. */
  lazy val cleared: SlotSchedule = updateWrapped(wrapped.mapValuesStrict(_.cleared))

  /** Get the records for a specific Topic */
  def on(t: Topic): Record = wrapped(t)

  def changeSlot(newSlot: Slot): SlotSchedule = copy(
    slot = newSlot,
    wrapped = wrapped.map { case (topic, record) => topic -> record.copy(slot = newSlot) }
  )

  /** Add a new record to this schedule. */
  def add(record: Record): SlotSchedule = updateWrapped(wrapped + (record.topic -> record))

  def addAll(records: Set[Record]): SlotSchedule = updateWrapped(wrapped ++ records.map(r => r.topic -> r))

  def addTopic(topic: Topic): SlotSchedule = updateWrapped(wrapped + (topic -> Record(slot, topic, topic.mandatory)))

  def addTopics(topics: Set[Topic]): SlotSchedule = updateWrapped(wrapped ++ topics.map(t => t -> Record(slot, t, t.mandatory)))

  def removeTopic(topic: Topic): SlotSchedule = updateWrapped(wrapped - topic)

  def removeTopics(topics: Set[Topic]): SlotSchedule = updateWrapped(wrapped -- topics)

  /** Replace an existing topic by a new one (typically unscheduled). Mandatory persons are set on the new topic and no
   * one else, so the schedule is probably unsound and/or unfilled. */
  def replaceTopic(oldTopic: Topic, newTopic: Topic): SlotSchedule = updateWrapped {
    wrapped - oldTopic + (newTopic -> Record(slot, newTopic, newTopic.mandatory))
  }

  def replaceTopics(oldTopics: Set[Topic], newTopics: Set[Topic]): SlotSchedule = updateWrapped {
    wrapped -- oldTopics ++ newTopics.map(t => t -> Record(slot, t, t.mandatory))
  }

  /** Adds a person to some topic already on schedule. If the topic is not on schedule, returns the same schedule. */
  def addPersonToExistingTopic(topic: Topic, person: Person): SlotSchedule = updateTopicRecord(topic)(_.addPerson(person))

  /** Swap two persons on a slot. Persons are in couple with there current topic. */
  def swapPersons(tp1: (Topic, Person), tp2: (Topic, Person)): SlotSchedule = updateWrapped {
    val (t1, p1) = tp1
    val (t2, p2) = tp2
    val newR1 = wrapped(t1).replacePerson(p1, p2)
    val newR2 = wrapped(t2).replacePerson(p2, p1)
    wrapped.updated(t1, newR1).updated(t2, newR2)
  }

  /** Score for this slot schedule if we swap those two persons. Persons are in couple with their current topic. */
  def deltaScoreIfSwapPersons(tp1: (Topic, Person), tp2: (Topic, Person)): Map[Person, Score] = {
    val (t1, p1) = tp1
    val (t2, p2) = tp2
    val oldR1 = wrapped(t1)
    val oldR2 = wrapped(t2)
    val newR1 = oldR1.replacePerson(p1, p2)
    val newR2 = oldR2.replacePerson(p2, p1)
    val persons = newR1.persons ++ newR2.persons
    // TODO ++ is a minor (6%) hot-spot
    persons.view.map { p =>
      val delta =
        newR1.unweightedScoresByPerson.getOrElse(p, Score.Zero) +
          newR2.unweightedScoresByPerson.getOrElse(p, Score.Zero) -
          oldR1.unweightedScoresByPerson.getOrElse(p, Score.Zero) -
          oldR2.unweightedScoresByPerson.getOrElse(p, Score.Zero)
      p -> delta
    }.toMap
    // TODO toMap is a major (12%) hot-spot
  }

  /** Move a person on some slot, from some topic to another one. */
  def movePerson(source: Topic, destination: Topic, person: Person): SlotSchedule = updateWrapped {
    val newSourceRecord = wrapped(source).removePerson(person)
    val newDestinationRecord = wrapped(destination).addPerson(person)
    wrapped.updated(source, newSourceRecord).updated(destination, newDestinationRecord)
  }

  /** Score for each person, regardless of its weight. All personal scores are records-level, so the whole computation is done per record. */
  lazy val unweightedScoresByPerson: Map[Person, Score] = recordsList.map(_.unweightedScoresByPerson).combineAll

  lazy val impersonalScoreTopicLevel: Score = Score.sum(recordsList)(_.impersonalScore)

  /** Impersonal score of the slot, regardless of how persons are assigned to topics on this slot (as long as the same persons are present) */
  lazy val impersonalScoreSlotLevel: Score = preferencesScoreRec(problem.impersonalSlotLevelPreferencesList)

  /** Total impersonal score in this slot. */
  lazy val impersonalScore: Score = impersonalScoreTopicLevel + impersonalScoreSlotLevel

  @tailrec
  private def preferencesScoreRec(prefs: List[Preference.SlotLevel], sum: Double = 0): Score = prefs match {
    case Nil => Score(sum)
    case p :: ps =>
      val s = p.scoreSlot(this)
      if (s.isNegativeInfinity) s else preferencesScoreRec(ps, sum + s.value)
  }

  /** This schedule makes sense. No person on multiple topics at the same time. */
  lazy val isSound: Boolean = {
    val persons = recordsList.flatMap(_.personsList) // Seq to keep duplicates, we're looking for them
    persons.size == persons.toSet.size
  }

  /** Partial schedules don't even have all of their topics planned yet (and they're obviously unfilled as well).
   * @return true if this respects all constraints applicable to partial schedules
   */
  lazy val isPartialSolution: Boolean = {
    lazy val recordsOk = records.forall(_.isUnfilledSolution) // Records don't have a partial status: if they exist, they're planned. They can only be unfilled.
    lazy val maxTopicsOk = slot.maxTopics >= topics.size
    lazy val constraintsOk = problem.slotLevelConstraints.forall { c => !c.isApplicableToPartialSchedule || c.isRespectedSlot(this) }
    recordsOk && maxTopicsOk && constraintsOk
  }

  /**
   * Unfilled Schedules are schedule where topics are matched, but not all persons are assigned yet.
   * @return true if this respects all constraints applicable to unfilled schedules
   */
  lazy val isUnfilledSolution: Boolean = isPartialSolution && {
    problem.slotLevelConstraints.forall { c => !c.isApplicableToUnfilledSchedule || c.isRespectedSlot(this) }
  }

  /** @return true if this respects all constraints */
  lazy val isSolution: Boolean = {
    isUnfilledSolution &&
      records.forall(_.isSolution) &&
      problem.slotLevelConstraints.forall { c => c.isApplicableToUnfilledSchedule || c.isRespectedSlot(this) }
  }

  /** Returns the errors on this slot-schedule. Slow, so avoid using it. */
  lazy val slowErrors: Seq[String] = {
    val recordsErrors = records.flatMap(_.slowErrors).toSeq

    val maxTopicsError = if (slot.maxTopics < topics.size) Some(s"Too many topics in slot ${slot.name}") else None

    val constraintsError = problem.slotLevelConstraints.flatMap { c =>
      if (!c.isRespectedSlot(this)) Some(s"Constraint $c not respected in slot ${slot.name}") else None
    }.toSeq

    val personPresentsErrors = {
      val personsActuallyPresent = records.flatMap(_.persons).toSet
      if (personsActuallyPresent == slot.personsPresent) Nil else {
        val missingPersons = slot.personsPresent -- personsActuallyPresent
        val missingPersonsError = if (missingPersons.isEmpty) Nil else Seq(s"Missing persons in slot ${slot.name}: ${missingPersons.map(_.name).mkString(", ")}")
        val unexpectedPersons = personsActuallyPresent -- slot.personsPresent
        val unexpectedPersonsError = if (unexpectedPersons.isEmpty) Nil else Seq(s"Unexpected persons in slot ${slot.name}: ${unexpectedPersons.map(_.name).mkString(", ")}")
        missingPersonsError ++ unexpectedPersonsError
      }
    }

    val duplicatePersonsError = {
      val duplicatePersons: Map[Person.Id, Iterable[Person]] = records.flatMap(_.persons.toSeq).groupBy(_.id).filter(_._2.size > 1)
      if (duplicatePersons.isEmpty) None else Some {
        s"Duplicate persons in slot ${slot.name}: ${duplicatePersons.map(_._2.head.name).mkString(", ")}"
      }
    }

    recordsErrors ++ maxTopicsError ++ constraintsError ++ personPresentsErrors ++ duplicatePersonsError
  }

  /** Produces a clear, multiline version of this schedule slot, with a 2-space indentation. */
  lazy val toFormattedString: String = {
    val builder = new StringBuilder("  ").append(slot.name).append("\n")
    recordsList.sortBy(_.topic.name).foreach { r =>
      builder.append("    ").append(r.toFormattedString).append("\n")
    }
    builder.toString
  }

  /** Unassign all persons, except mandatory persons. If unassigned topics do not exist, returns this SlotSchedule without change. Used only in tests. */
  @testOnly def unassignAll: SlotSchedule =
    if (!problem.hasUnassignedTopics) this
    else updateWrapped {
      val cleanedWrapped = wrapped.collect {
        case (topic, _) if !topic.isUnassigned => topic -> Record(slot, topic, topic.mandatory)
      }
      val unassignedTopic = problem.unassignedTopics(slot)
      val unassignedPersons = slot.personsPresent -- cleanedWrapped.values.view.flatMap(_.persons).toSet
      val unassignedRecord = Record(slot, unassignedTopic, unassignedPersons)
      cleanedWrapped + (unassignedTopic -> unassignedRecord)
    }

  /** Merge with another slot schedule's content. Used only in tests. */
  @testOnly def ++(that: SlotSchedule): SlotSchedule = updateWrapped {
    if (slot != that.slot) throw new IllegalArgumentException
    (records ++ that.records).groupBy(_.topic).mapValuesStrict(_.reduce(_ ++ _))
  }

  override def equals(o: Any): Boolean = o match {
    case that: SlotSchedule => this.slot == that.slot && this.wrapped == that.wrapped
    case _ => false
  }

  override lazy val hashCode: Int = wrapped.hashCode * 17 + slot.hashCode
}

object SlotSchedule {

  def empty(slot: Slot)(using problem: Problem): SlotSchedule = SlotSchedule(slot, Map.empty)

  /** Slot schedule where everyone is on an "unassigned" topic, no other topic. Doesn't work if unassigned isn't allowed. */
  def everyoneUnassigned(slot: Slot)(using problem: Problem): SlotSchedule =
    if (problem.hasUnassignedTopics) {
      val t = problem.unassignedTopics(slot)
      SlotSchedule(slot, Map(t -> Record(slot, t, slot.personsPresent)))
    } else {
      throw new IllegalStateException("Cannot call everyoneUnassigned, persons cannot be unassigned")
    }

  /** Commodity method */
  @testOnly def from(slot: Slot, entries: Record*)(using problem: Problem): SlotSchedule = {
    new SlotSchedule(slot, entries.groupBy(_.topic).mapValuesStrict(_.reduce(_ ++ _)))
  }
}
