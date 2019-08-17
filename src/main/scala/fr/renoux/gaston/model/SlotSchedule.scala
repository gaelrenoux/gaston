package fr.renoux.gaston.model

import fr.renoux.gaston.util.CollectionImplicits._
import fr.renoux.gaston.util.testOnly
import scalaz.Scalaz._

import scala.annotation.tailrec

/** A schedule for a specific slot */
case class SlotSchedule(slot: Slot, records: Set[Record])(implicit val problem: Problem
) {

  @inline private[model] def updateRecords(f: Set[Record] => Set[Record]): SlotSchedule =
    copy(records = f(records))

  @inline private[model] def mapRecords(f: Record => Record): SlotSchedule =
    updateRecords(_.map(f))

  @inline private[model] def partialMapRecords(f: PartialFunction[Record, Record]): SlotSchedule =
    updateRecords(_.map { r => f.applyOrElse(r, identity[Record]) })

  lazy val isEmpty: Boolean = records.isEmpty

  lazy val recordsList: List[Record] = records.toList
  lazy val recordsPerTopic: Map[Topic, Record] = records.map(r => r.topic -> r).toMap
  lazy val recordsThatCanRemovePersons: Set[Record] = records.filter(_.canRemovePersons)
  lazy val recordsThatCanAddPersons: Set[Record] = records.filter(_.canAddPersons)

  lazy val topics: Set[Topic] = records.map(_.topic)
  lazy val topicsList: List[Topic] = topics.toList
  lazy val realTopics: Set[Topic] = topics.filterNot(_.virtual)
  lazy val realTopicsList: List[Topic] = realTopics.toList
  lazy val removableTopicsList: List[Topic] = realTopicsList.filterNot(_.forced)

  /** Topics that cannot be added on this slot, because of the slot itself */
  lazy val permanentlyIncompatibleTopics: Set[Topic] = problem.incompatibleTopicsPerSlot(slot)

  /** Topics that cannot be added on this slot as it now (but may be added later if the configuration of the slot changes) */
  lazy val currentlyIncompatibleTopics: Set[Topic] = topics.flatMap(problem.incompatibleTopicsPerTopic)

  /** Topics that cannot be added on this slot, because of the slot or other topics */
  lazy val incompatibleTopics: Set[Topic] = permanentlyIncompatibleTopics ++ currentlyIncompatibleTopics

  lazy val countTopics: Int = topics.size
  lazy val maxTopicsLeft: Int = slot.maxTopics - countTopics

  lazy val minPersons: Option[Int] = if (isEmpty) None else Some(topics.view.map(_.min).sum)
  lazy val maxPersons: Option[Int] = if (isEmpty) None else Some(topics.view.map(_.max).sum)

  lazy val scheduledPersons: Set[Person] = records.flatMap(_.persons)
  lazy val unscheduledPersons: Set[Person] = slot.personsPresent -- scheduledPersons
  lazy val unscheduledPersonsList: List[Person] = unscheduledPersons.toList
  lazy val personsPerTopic: Map[Topic, Set[Person]] = records.groupBy(_.topic).mapValuesStrict(_.flatMap(_.persons))
  lazy val countPersonsPerTopic: Map[Topic, Int] = personsPerTopic.mapValuesStrict(_.size)
  lazy val personGroups: Iterable[Set[Person]] = records.view.map(_.persons).toList // not a Set: we do not want to deduplicate identical groups!
  lazy val mandatory: Set[Person] = topics.flatMap(_.mandatory)

  lazy val isMinPersonsTooHigh: Boolean = minPersons.exists(_ > problem.personsCount)
  lazy val isMaxPersonsTooLow: Boolean = maxPersons.exists(_ < problem.personsCount)

  /** Clear all non-mandatory persons. Returned schedule is partial, obviously. */
  lazy val cleared: SlotSchedule = mapRecords { r => r.copy(persons = r.topic.mandatory) }

  /** Get the Persons for a specific Topic */
  def on(t: Topic): Record = recordsPerTopic(t)

  /** Add a new record to this schedule. */
  def add(record: Record): SlotSchedule = updateRecords(_ + record)

  def addAll(records: Set[Record]): SlotSchedule = updateRecords(_ ++ records)

  def removeTopic(topic: Topic): SlotSchedule = updateRecords(_.filter(_.topic != topic))

  def removeTopics(topics: Set[Topic]): SlotSchedule = updateRecords(_.filterNot(r => topics.contains(r.topic)))

  /** Replace an existing topic by a new one (typically unscheduled). Mandatory persons are set on the new topic and no
    * one else, so the schedule is probably unsound and/or partial. */
  def replaceTopic(oldTopic: Topic, newTopic: Topic): SlotSchedule = partialMapRecords {
    case r: Record if r.topic == oldTopic => r.copy(topic = newTopic, persons = newTopic.mandatory)
  }

  def replaceTopics(oldTopics: Set[Topic], newTopics: Set[Topic]): SlotSchedule = updateRecords { records =>
    records.filterNot(r => oldTopics.contains(r.topic)) ++ newTopics.map(t => Record(slot, t, t.mandatory))
  }

  /** Adds a person to some topic already on schedule. If the topic is not on schedule, returns the same schedule. */
  def addPersonToExistingTopic(topic: Topic, person: Person): SlotSchedule = partialMapRecords {
    case r: Record if r.topic == topic => r.copy(persons = r.persons + person)
  }

  /** Swap two persons on a slot. Persons are in couple with there current topic. */
  def swapPersons(tp1: (Topic, Person), tp2: (Topic, Person)): SlotSchedule = updateRecords { records =>
    val (t1, p1) = tp1
    val (t2, p2) = tp2
    val r1 = recordsPerTopic(t1)
    val r2 = recordsPerTopic(t2)
    val newR1 = r1.copy(persons = r1.persons - p1 + p2)
    val newR2 = r2.copy(persons = r2.persons - p2 + p1)
    records - r1 - r2 + newR1 + newR2
  }

  /** Move a person on some slot, from some topic to another one. */
  def movePerson(source: Topic, destination: Topic, person: Person): SlotSchedule = updateRecords { records =>
    val sourceRecord = recordsPerTopic(source)
    val destinationRecord = recordsPerTopic(destination)
    val newSourceRecord = sourceRecord.copy(persons = sourceRecord.persons - person)
    val newDestinationRecord = destinationRecord.copy(persons = destinationRecord.persons + person)
    records - sourceRecord - destinationRecord + newSourceRecord + newDestinationRecord
  }

  /** Score for each person, regardless of its weight. All personal scores are records-level, so the whole computation is done per record. */
  lazy val unweightedScoresByPerson: Map[Person, Score] = recordsList.map(_.unweightedScoresByPerson).suml

  lazy val impersonalScore: Score =
    recordsList.map(_.impersonalScore).suml + preferencesScoreRec(problem.impersonalSlotLevelPreferencesList)

  @tailrec
  private def preferencesScoreRec(prefs: List[Preference.SlotLevel], sum: Double = 0): Score = prefs match {
    case Nil => Score(sum)
    case p :: ps =>
      val s = p.scoreSlot(this)
      if (s.value == Double.NegativeInfinity) s else preferencesScoreRec(ps, sum + s.value)
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
  lazy val isSolution: Boolean =
    isPartialSolution &&
      records.forall(_.isSolution)
      problem.slotLevelConstraints.forall { c => c.isApplicableToPartialSchedule || c.isRespectedSlot(this) }

  /** Produces a clear, multiline version of this schedule slot, with a 2-space indentation. */
  lazy val toFormattedString: String = {
    val builder = new StringBuilder("  ").append(slot.name).append(": \n")
    recordsList.sortBy(_.topic.name).foreach { r =>
      builder.append("    ").append(r.toFormattedString).append("\n")
    }
    builder.toString
  }

  /** Merge with another slot schedule's content. Used only in tests. */
  @testOnly def ++(that: SlotSchedule): SlotSchedule = {
    if (slot != that.slot) throw new IllegalArgumentException
    copy(records = (records ++ that.records).groupBy(_.topic).mapValuesStrict(_.reduce(_ ++ _)).values.toSet)
  }
}

object SlotSchedule {
  def empty(slot: Slot)(implicit problem: Problem): SlotSchedule = SlotSchedule(slot, Set.empty[Record])

  /** Schedule where everyone is on an "unassigned" topic */
  def everyoneUnassigned(slot: Slot)(implicit problem: Problem): SlotSchedule =
    SlotSchedule(slot, Set(Record(slot, Topic.unassigned(slot), slot.personsPresent)))

  /** Commodity method */
  def apply(slot: Slot, entries: Seq[Record]*)(implicit problem: Problem): SlotSchedule = {
    new SlotSchedule(slot, entries.flatten.toSet)
  }
}