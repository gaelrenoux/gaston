package fr.renoux.gaston.model

import cats.implicits._
import fr.renoux.gaston.util.CollectionImplicits._
import fr.renoux.gaston.util.testOnly

import scala.annotation.tailrec

/** A schedule for a specific slot */
final case class SlotSchedule(
    slot: Slot,
    private val wrapped: Map[Topic, Record]
)(implicit val problem: Problem) {

  @inline private def updateWrapped(w: Map[Topic, Record]): SlotSchedule =
    copy(wrapped = w)

  @inline private def updateTopicRecord(topic: Topic)(f: Record => Record): SlotSchedule = {
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
  lazy val realTopics: Iterable[Topic] = topics.filterNot(_.virtual)
  lazy val realTopicsSet: Set[Topic] = realTopics.toSet
  lazy val realTopicsList: List[Topic] = realTopics.toList
  lazy val removableTopics: Iterable[Topic] = realTopics.filterNot(_.forced)
  lazy val removableTopicsSet: Set[Topic] = realTopics.filterNot(_.forced).toSet

  lazy val persons: Iterable[Person] = wrapped.values.flatMap(_.persons)

  /** Topics that cannot be added on this slot, because of the slot itself */
  lazy val permanentlyIncompatibleTopics: Set[Topic] = problem.incompatibleTopicsBySlot(slot)

  /** Topics that cannot be added on this slot as it now (but may be added later if the configuration of the slot changes) */
  lazy val currentlyIncompatibleTopics: Set[Topic] = topicsSet.flatMap(problem.incompatibleTopicsByTopic)

  /** Topics that cannot be added on this slot, because of the slot or other topics */
  lazy val incompatibleTopics: Set[Topic] = permanentlyIncompatibleTopics ++ currentlyIncompatibleTopics

  lazy val countTopics: Int = topics.size
  lazy val maxTopicsLeft: Int = slot.maxTopics - countTopics

  lazy val minPersons: Option[Int] = if (isEmpty) None else Some(topics.view.map(_.min).sum)
  lazy val maxPersons: Option[Int] = if (isEmpty) None else Some(topics.view.map(_.max).sum)

  lazy val scheduledPersons: Set[Person] = recordsSet.flatMap(_.persons)
  lazy val unscheduledPersons: Set[Person] = slot.personsPresent -- scheduledPersons
  lazy val unscheduledPersonsList: List[Person] = unscheduledPersons.toList
  lazy val personsByTopic: Map[Topic, Set[Person]] = recordsSet.groupBy(_.topic).mapValuesStrict(_.flatMap(_.persons))
  lazy val countPersonsByTopic: Map[Topic, Int] = personsByTopic.mapValuesStrict(_.size)
  lazy val personGroups: Iterable[Set[Person]] = records.view.map(_.persons).toList // not a Set: we do not want to deduplicate identical groups!
  lazy val mandatory: Set[Person] = topicsSet.flatMap(_.mandatory)

  lazy val isMinPersonsTooHigh: Boolean = minPersons.exists(_ > problem.personsCount)
  lazy val isMaxPersonsTooLow: Boolean = maxPersons.exists(_ < problem.personsCount)

  /** Clear all non-mandatory persons. Returned schedule is partial, obviously. */
  lazy val cleared: SlotSchedule = updateWrapped(wrapped.mapValuesStrict(_.cleared))

  /** Get the records for a specific Topic */
  def on(t: Topic): Record = wrapped(t)

  /** Add a new record to this schedule. */
  def add(record: Record): SlotSchedule = updateWrapped(wrapped + (record.topic -> record))

  def addAll(records: Set[Record]): SlotSchedule = updateWrapped(wrapped ++ records.map(r => r.topic -> r))

  def addTopic(topic: Topic): SlotSchedule = updateWrapped(wrapped + (topic -> Record(slot, topic, topic.mandatory)))

  def addTopics(topics: Set[Topic]): SlotSchedule = updateWrapped(wrapped ++ topics.map(t => t -> Record(slot, t, t.mandatory)))

  def removeTopic(topic: Topic): SlotSchedule = updateWrapped(wrapped - topic)

  def removeTopics(topics: Set[Topic]): SlotSchedule = updateWrapped(wrapped -- topics)

  /** Replace an existing topic by a new one (typically unscheduled). Mandatory persons are set on the new topic and no
    * one else, so the schedule is probably unsound and/or partial. */
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

  /** Score for this slot schedule if we swap those two persons. Persons are in couple with there current topic. */
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

  lazy val impersonalScoreTopicLevel: Score = recordsList.view.map(_.impersonalScore).sum

  /** Impersonal score of the slot, regardless of how persons are assigned to topics on this slot (as long as the same persons are present) */
  lazy val impersonalScoreSlotLevel: Score = preferencesScoreRec(problem.impersonalSlotLevelPreferencesList)

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

  /**
    * Partial Schedules are schedule where topics are matched, but not all persons are assigned yet.
    * @return true if this respects all constraints applicable to partial schedules
    */
  lazy val isPartialSolution: Boolean = {
    lazy val recordsOk = records.forall(_.isPartialSolution)
    lazy val maxTopicsOk = slot.maxTopics >= topics.size
    lazy val constraintsOk = problem.slotLevelConstraints.forall { c => !c.isApplicableToPartialSchedule || c.isRespectedSlot(this) }
    recordsOk && maxTopicsOk && constraintsOk
  }

  /** @return true if this respects all constraints */
  lazy val isSolution: Boolean = {
    isPartialSolution &&
      records.forall(_.isSolution) &&
      problem.slotLevelConstraints.forall { c => c.isApplicableToPartialSchedule || c.isRespectedSlot(this) }
  }

  lazy val errors: Seq[String] = if (isSolution) Nil else {
    val recordsErrors = records.flatMap(_.errors).toSeq
    val maxTopicsError = if (slot.maxTopics < topics.size) Some(s"Too many topics in slot ${slot.name}") else None
    val constraintsError = problem.slotLevelConstraints.flatMap { c =>
      if (!c.isRespectedSlot(this)) Some(s"Constraint $c not respected in slot ${slot.name}") else None
    }.toSeq
    recordsErrors ++ maxTopicsError ++ constraintsError
  }

  /** Produces a clear, multiline version of this schedule slot, with a 2-space indentation. */
  lazy val toFormattedString: String = {
    val builder = new StringBuilder("  ").append(slot.name).append(": \n")
    recordsList.sortBy(_.topic.name).foreach { r =>
      if (r.persons.nonEmpty) {
        builder.append("    ").append(r.toFormattedString).append("\n")
      }
    }
    builder.toString
  }

  /** Merge with another slot schedule's content. Used only in tests. */
  @testOnly def ++(that: SlotSchedule): SlotSchedule = updateWrapped {
    if (slot != that.slot) throw new IllegalArgumentException
    (records ++ that.records).groupBy(_.topic).mapValuesStrict(_.reduce(_ ++ _))
  }

  override def equals(o: Any): Boolean = o match {
    case that: SlotSchedule => this.slot == that.slot && this.wrapped == that.wrapped
  }

  override lazy val hashCode: Int = wrapped.hashCode * 17 + slot.hashCode
}

object SlotSchedule {
  def empty(slot: Slot)(implicit problem: Problem): SlotSchedule = SlotSchedule(slot, Map.empty)

  /** Slot schedule where everyone is on an "unassigned" topic */
  def everyoneUnassigned(slot: Slot)(implicit problem: Problem): SlotSchedule = {
    val t = problem.unassignedTopics(slot)
    SlotSchedule(slot, Map(t -> Record(slot, t, slot.personsPresent)))
  }

  /** Commodity method */
  @testOnly def from(slot: Slot, entries: Seq[Record]*)(implicit problem: Problem): SlotSchedule = {
    new SlotSchedule(slot, entries.flatten.groupBy(_.topic).mapValuesStrict(_.reduce(_ ++ _)))
  }
}
