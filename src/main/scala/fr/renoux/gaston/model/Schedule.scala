package fr.renoux.gaston.model

import fr.renoux.gaston.util.CollectionImplicits._

/**
  * A schedule is an association of people, to topics, to slots.
  * What we're trying and testing and looking for a good one.
  */
case class Schedule(
    private val wrapped: Set[Record]
)(implicit
    val problem: Problem
) {

  @inline private def updateRecords(f: Set[Record] => Set[Record]): Schedule =
    copy(wrapped = f(records))

  lazy val records: Set[Record] = wrapped
  lazy val slots: Set[Slot] = records.map(_.slot)
  lazy val recordsPerSlot: Map[Slot, Set[Record]] = records.groupBy(_.slot)
  lazy val slotSchedulesMap: Map[Slot, SlotSchedule] = slots.zipWith(SlotSchedule(this, _)).toMap
  lazy val slotSchedules: Iterable[SlotSchedule] = slotSchedulesMap.values
  lazy val personsPerSlot: Map[Slot, Set[Person]] = recordsPerSlot.mapValuesStrict { x => x.flatMap(_.persons) }
  lazy val personsPerTopic: Map[Topic, Set[Person]] = records.groupBy(_.topic).mapValuesStrict { x => x.flatMap(_.persons) }
  lazy val topicsPerSlot: Map[Slot, Set[Topic]] = recordsPerSlot.mapValuesStrict { x => x.map(_.topic) }
  lazy val countPersonsPerTopic: Map[Topic, Int] = personsPerTopic.mapValuesStrict(_.size)
  lazy val topicToSlot: Map[Topic, Slot] = topicsPerSlot.flatMap { case (s, ts) => ts.map(_ -> s) }
  lazy val personGroups: Iterable[Set[Person]] = personsPerTopic.values //not a Set: we do not want to deduplicate identical groups!

  /** Get the SlotSchedule for a specific Slot */
  def on(slot: Slot): SlotSchedule = SlotSchedule(this, slot)

  /** Add a new record to this schedule. */
  def add(record: Record): Schedule = updateRecords(_ + record)

  def +(record: Record): Schedule = updateRecords(_ + record)

  /** Merge with another schedule's content. */
  def merge(that: Schedule): Schedule = {
    val cumulatedRecords = records ++ that.records
    val mergedMap = cumulatedRecords.groupBy(t => (t.slot, t.topic)).mapValuesStrict(_.flatMap(_.persons))
    val mergedRecords = mergedMap.toSet.map(Record.fromTuple2)
    copy(wrapped = mergedRecords)
  }

  def ++(that: Schedule): Schedule = merge(that)

  /** Adds a person to some topic already on schedule. If the topic is not on schedule, returns the same schedule. */
  def addPersonToExistingTopic(topic: Topic, person: Person): Schedule = updateRecords(_.map {
    case Record(s, t, ps) if t == topic => Record(s, t, ps + person)
    case r => r
  })

  /** Swap two persons on a slot. Persons are in couple with there current record. */
  def swapPersons(rp1: (Record, Person), rp2: (Record, Person)): Schedule = updateRecords { records =>
    val (r1, p1) = rp1
    val (r2, p2) = rp2
    assert(r1.slot == r2.slot)
    val newR1 = r1.copy(persons = r1.persons - p1 + p2)
    val newR2 = r2.copy(persons = r2.persons - p2 + p1)
    records - r1 - r2 + newR1 + newR2
  }

  /** Move a person from some record to another one. */
  def movePerson(source: Record, destination: Record, person: Person): Schedule = updateRecords { records =>
    assert(source.slot == destination.slot)
    val newSource = source.copy(persons = source.persons - person)
    val newDest = destination.copy(persons = destination.persons + person)
    records - source - destination + newSource + newDest
  }

  /** The schedule makes sense. No person on multiple topics at the same time. */
  lazy val isSound: Boolean = {
    records.groupBy(_.slot).values.forall { records =>
      val persons = records.toSeq.flatMap(_.persons.toSeq) //toSeq to keep duplicates, we're looking for them
      persons.size == persons.toSet.size
    }
  }

  /** Score for each person, regardless of its weight. */
  lazy val unweightedScoresByPerson: Map[Person, Score] = {
    val individualScores = problem.preferences.toSeq.map(p => p -> p.score(this))
    individualScores.groupBy(_._1.person).mapValuesStrict(_.map(_._2).sum)
  }

  /**
    * Partial Schedules are schedule where slots and topics are matched, but not all persons are assigned yet.
    * @return true if this respects all constraints applicable to partial schedules
    */
  lazy val isPartialSolution: Boolean = {
    problem.constraints.forall { c => !c.isApplicableToPartialSchedule || c.isRespected(this) }
  }

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

    for ((slot, personsPerTopic) <- personsPerTopicPerSlot) {
      builder.append("  ").append(slot).append(": \n")
      for ((topic, persons) <- personsPerTopic) {
        builder.append("    ").append(topic).append(": ").append(persons.mkString("", ", ", "\n"))
      }
    }

    builder.toString
  }

}

object Schedule {

  /** Empty schedule for a problem */
  def empty(implicit problem: Problem): Schedule = Schedule()

  def apply(entries: Seq[Record]*)(implicit problem: Problem): Schedule = new Schedule(entries.flatten.toSet)
}
