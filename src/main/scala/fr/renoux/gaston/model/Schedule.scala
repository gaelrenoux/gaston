package fr.renoux.gaston.model

import fr.renoux.gaston.engine.Context
import fr.renoux.gaston.util.CollectionImplicits._
import Context._

/**
  * A schedule is an association of people, to topics, to slots.
  * What we're trying and testing and looking for a good one.
  */
case class Schedule(
    private val wrapped: Set[Record]
)(implicit
    val problem: Problem,
    ctx: Context
) {

  @inline private def updateRecords(f: Set[Record] => Set[Record]): Schedule =
    copy(wrapped = f(records))

  @inline private def partialMapRecords(f: PartialFunction[Record, Record]): Schedule =
    updateRecords(_.map { r => f.applyOrElse(r, identity[Record]) })

  lazy val records: Set[Record] = wrapped
  lazy val slots: Set[Slot] = records.map(_.slot)
  lazy val recordsPerSlot: Map[Slot, Set[Record]] = records.groupBy(_.slot)
  lazy val recordsPerSlotPerTopic: Map[Slot, Map[Topic, Set[Record]]] = recordsPerSlot.mapValuesStrict(_.groupBy(_.topic))
  lazy val slotSchedulesMap: Map[Slot, SlotSchedule] = slots.zipWith(SlotSchedule(this, _)).toMap
  lazy val slotSchedules: Iterable[SlotSchedule] = slotSchedulesMap.values
  lazy val personsPerSlot: Map[Slot, Set[Person]] = recordsPerSlot.mapValuesStrict { x => x.flatMap(_.persons) }
  lazy val personsPerTopic: Map[Topic, Set[Person]] = records.groupBy(_.topic).mapValuesStrict { x => x.flatMap(_.persons) }
  lazy val topicsPerSlot: Map[Slot, Set[Topic]] = recordsPerSlot.mapValuesStrict { x => x.map(_.topic) }
  lazy val countTopicsPerSlot: Map[Slot, Int] = topicsPerSlot.mapValuesStrict(_.size)
  lazy val countPersonsPerTopic: Map[Topic, Int] = personsPerTopic.mapValuesStrict(_.size)
  lazy val topicToSlot: Map[Topic, Slot] = topicsPerSlot.flatMap { case (s, ts) => ts.map(_ -> s) }
  lazy val personGroups: Iterable[Set[Person]] = personsPerTopic.values // not a Set: we do not want to deduplicate identical groups!

  lazy val maxPersonsOnSlot: Map[Slot, Int] = topicsPerSlot.mapValuesStrict(_.view.map(_.max).sum)
  lazy val minPersonsOnSlot: Map[Slot, Int] = topicsPerSlot.mapValuesStrict(_.view.map(_.min).sum)
  lazy val mandatoryPersonsOnSlot: Map[Slot, Set[Person]] = topicsPerSlot.mapValuesStrict(_.flatMap(_.mandatory))

  lazy val scheduledTopics: Set[Topic] = records.map(_.topic)
  lazy val unscheduledTopics: Set[Topic] = (problem.topics -- scheduledTopics).filter(_.removable)

  lazy val recordsSeq: Seq[Record] = records.toSeq
  lazy val scheduledTopicsSeq: Seq[Topic] = scheduledTopics.toSeq
  lazy val unscheduledTopicsSeq: Seq[Topic] = unscheduledTopics.toSeq

  lazy val score: Score = if (records.isEmpty) Score.MinValue else Scorer.score(this)

  /** Get the SlotSchedule for a specific Slot */
  def on(slot: Slot): SlotSchedule = SlotSchedule(this, slot) // TODO cache this

  /** Add a new record to this schedule. */
  def add(record: Record): Schedule = updateRecords(_ + record)

  def +(record: Record): Schedule = updateRecords(_ + record)

  def add(records: Set[Record]): Schedule = if (records.isEmpty) this else updateRecords(_ ++ records)

  def ++(records: Set[Record]): Schedule = if (records.isEmpty) this else updateRecords(_ ++ records)

  /** Merge with another schedule's content. */
  def merge(that: Schedule): Schedule = {
    val cumulatedRecords = records ++ that.records
    val mergedMap = cumulatedRecords.groupBy(t => (t.slot, t.topic)).mapValuesStrict(_.flatMap(_.persons))
    val mergedRecords = mergedMap.toSet.map(Record.fromTuple2)
    copy(wrapped = mergedRecords)
  }

  def ++(that: Schedule): Schedule = merge(that)

  /** Clear all non-mandatory persons on the given slots. Returned schedule is partial, obviously. */
  def clearSlots(slots: Slot*): Schedule = {
    val slotsSet = slots.toSet
    partialMapRecords {
      case Record(s, t, _) if slotsSet(s) => Record(s, t, t.mandatory)
    }
  }

  /** Swap two topics from two different slots. Mandatory persons are set on the new topics and no one else, so the
    * schedule is probably unsound and/or partial. */
  def swapTopic(st1: (Slot, Topic), st2: (Slot, Topic)): Schedule = partialMapRecords {
    case Record(s, t, _) if (s, t) == st1 => Record(s, st2._2, st2._2.mandatory) // TODO should probably have a method that corrects the schedule
    case Record(s, t, _) if (s, t) == st2 => Record(s, st1._2, st1._2.mandatory)
  }

  /** Swap two groups of topics from two different slots. Mandatory persons are set on the new topics and no one else, so the
    * schedule is probably unsound and/or partial. */
  def swapTopics(st1: (Slot, Set[Topic]), st2: (Slot, Set[Topic])): Schedule = updateRecords { records =>
    val all = st1._2 ++ st2._2
    records.filterNot(r => all.contains(r.topic)) ++
      st1._2.map(t => Record(st2._1, t, t.mandatory)) ++
      st2._2.map(t => Record(st1._1, t, t.mandatory))
  }

  /** Replace an existing topic by a new one (typically unscheduled, on a slot). Mandatory persons are set on the new
    * topic and no one else, so the schedule is probably unsound and/or partial. */
  def replaceTopic(oldTopic: Topic, newTopic: Topic): Schedule = partialMapRecords {
    case Record(s, t, _) if t == oldTopic => Record(s, newTopic, newTopic.mandatory)
  }

  def replaceTopics(slot: Slot, oldTopics: Set[Topic], newTopics: Set[Topic]): Schedule = updateRecords { records =>
    records.filterNot(r => oldTopics.contains(r.topic)) ++
      newTopics.map(t => Record(slot, t, t.mandatory))
  }

  def removeTopic(topic: Topic): Schedule = updateRecords(_.filter(_.topic != topic))

  def removeTopics(topics: Set[Topic]): Schedule = updateRecords(_.filterNot(r => topics.contains(r.topic)))


  /** Adds a person to some topic already on schedule. If the topic is not on schedule, returns the same schedule. */
  def addPersonToExistingTopic(topic: Topic, person: Person): Schedule = partialMapRecords {
    case Record(s, t, ps) if t == topic => Record(s, t, ps + person)
  }

  /** Swap two persons on a slot. Persons are in couple with there current topic. */
  def swapPersons(slot: Slot, tp1: (Topic, Person), tp2: (Topic, Person)): Schedule = updateRecords { records =>
    val (t1, p1) = tp1
    val (t2, p2) = tp2
    val r1 = recordsPerSlotPerTopic(slot)(t1).head
    val r2 = recordsPerSlotPerTopic(slot)(t2).head
    val newR1 = r1.copy(persons = r1.persons - p1 + p2)
    val newR2 = r2.copy(persons = r2.persons - p2 + p1)
    records - r1 - r2 + newR1 + newR2
  }

  /** Move a person on some slot, from some topic to another one. */
  def movePerson(slot: Slot, source: Topic, destination: Topic, person: Person): Schedule = updateRecords { records =>
    val sourceRecord = recordsPerSlotPerTopic(slot)(source).head
    val destinationRecord = recordsPerSlotPerTopic(slot)(destination).head
    val newSourceRecord = sourceRecord.copy(persons = sourceRecord.persons - person)
    val newDestinationRecord = destinationRecord.copy(persons = destinationRecord.persons + person)
    records - sourceRecord - destinationRecord + newSourceRecord + newDestinationRecord
  }

  /** The schedule makes sense. No person on multiple topics at the same time. No topic on multiple slots. */
  lazy val isSound: Boolean = {
    lazy val noUbiquity = recordsPerSlot.values.forall { recordsOnSlot =>
      val persons = recordsOnSlot.toSeq.flatMap(_.personsSeq) // Seq to keep duplicates, we're looking for them
      persons.size == persons.toSet.size
    }
    lazy val noDuplicates = {
      val topicsSeq = topicsPerSlot.values.flatten
      topicsSeq.size == topicsSeq.toSet.size
    }
    noUbiquity && noDuplicates
  }

  /** Score for each person, regardless of its weight. */
  lazy val unweightedScoresByPerson: Map[Person, Score] = {
    val individualScores = chrono("Schedule > unweightedScoresByPerson > individualScores") {
      problem.preferencesSeq.collect {
        case p: Preference.Personal => p -> p.score(this)
      }
    }

    chrono("Schedule > unweightedScoresByPerson > sum") {
      individualScores.groupBy(_._1.person).mapValuesStrict(_.foldLeft(Score.Zero)(_ + _._2))
    }
  }

  lazy val unpersonalScore: Score =
    problem.preferencesSeq.map {
      case _: Preference.Personal => Score.Zero
      case p: Preference => p.score(this)
    }.sum

  /**
    * Partial Schedules are schedule where slots and topics are matched, but not all persons are assigned yet.
    * @return true if this respects all constraints applicable to partial schedules
    */
  lazy val isPartialSolution: Boolean = {
    problem.constraints.forall { c => !c.isApplicableToPartialSchedule || c.isRespected(this) }
  }

  lazy val brokenPartialConstraints: Set[Constraint] =
    problem.constraints.filterNot { c => !c.isApplicableToPartialSchedule || c.isRespected(this) }

  /** @return true if this respects all constraints */
  lazy val isSolution: Boolean = {
    problem.constraints.forall(_.isRespected(this))
  }

  /** Produces a clear, multiline version of this schedule. */
  lazy val toFormattedString: String = {
    val builder = new StringBuilder("Schedule:\n")

    val personsPerTopicPerSlot = records.groupBy(_.slot.name).mapValuesStrict {
      _.groupBy(_.topic.name).mapValuesStrict {
        _.flatMap(_.persons).map(_.name).toSeq.sorted
      }.toSeq.sortBy(_._1)
    }.toSeq.sortBy(_._1)

    for {
      (slot, personsPerTopic) <- personsPerTopicPerSlot
    } {
      builder.append("  ").append(slot).append(": \n")
      for ((topic, persons) <- personsPerTopic) {
        builder.append("    ").append(topic).append(": ").append(persons.mkString("", ", ", "\n"))
      }
    }

    builder.append(unscheduledTopics.map(_.name).mkString("Unscheduled topics: ", ", ", "\n"))

    builder.toString
  }

}

object Schedule {

  implicit object ScheduleIsOrdered extends Ordering[Schedule] {
    override def compare(x: Schedule, y: Schedule): Int = x.score.compare(y.score)
  }

  /** Empty schedule for a problem */
  def empty(implicit problem: Problem, ctx: Context): Schedule = Schedule()

  /** Schedule where everyone is on an "unassigned" topic */
  def everyoneUnassigned(implicit problem: Problem, ctx: Context): Schedule = {
    Schedule(
      problem.slots.map { s =>
        Record(s, Topic.unassigned(s), problem.personsPerSlot(s))
      }
    )
  }

  /** Commodity method */
  def apply(entries: Seq[Record]*)(implicit problem: Problem, ctx: Context): Schedule = new Schedule(entries.flatten.toSet)
}
