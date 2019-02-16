package fr.renoux.gaston.model

import fr.renoux.gaston.util.CollectionImplicits._

/**
  * A schedule is an association of people, to topics, to slots.
  * What we're trying and testing and looking for a good one.
  */
case class Schedule(
    records: Set[Schedule.Record]
)(implicit
    val problem: Problem
) {


  import fr.renoux.gaston.model.Schedule._

  lazy val slots: Set[Slot] = records.map(_.slot)
  lazy val recordsPerSlot: Map[Slot, Set[Record]] = records.groupBy(_.slot)
  lazy val slotSchedulesMap = slots.zipWith(SlotSchedule(this, _)).toMap
  lazy val slotSchedules: Iterable[SlotSchedule] = slotSchedulesMap.values
  lazy val personsPerSlot: Map[Slot, Set[Person]] = recordsPerSlot.mapValuesStrict { x => x.flatMap(_.persons) }.withDefaultValue(Set())
  lazy val personsPerTopic: Map[Topic, Set[Person]] = records.groupBy(_.topic).mapValuesStrict { x => x.flatMap(_.persons) }.withDefaultValue(Set())
  lazy val topicsPerSlot: Map[Slot, Set[Topic]] = recordsPerSlot.mapValuesStrict { x => x.map(_.topic) }.withDefaultValue(Set())
  lazy val countPersonsPerTopic: Map[Topic, Int] = personsPerTopic.mapValuesStrict(_.size).withDefaultValue(0)
  lazy val topicToSlot: Map[Topic, Slot] = topicsPerSlot.flatMap { case (s, ts) => ts.map(_ -> s) }
  lazy val personGroups: Iterable[Set[Person]] = personsPerTopic.values //not a Set: we do not want to deduplicate identical groups!

  /** Get the SlotSchedule for a specific Slot */
  def on(slot: Slot): SlotSchedule = SlotSchedule(this, slot)

  /** Update the records from the schedule. */
  def updateRecords(f: Set[Schedule.Record] => Set[Schedule.Record]): Schedule = copy(records = f(records))

  /** Merge more triplets into this schedule. */
  def merge(addedRecords: Set[Record]): Schedule = {
    val cumulatedRecords = records ++ addedRecords
    val mergedMap = cumulatedRecords.groupBy(t => (t.slot, t.topic)).mapValuesStrict(_.flatMap(_.persons))
    val mergedRecords = mergedMap.toSet.map(Record.fromTuple2)
    copy(records = mergedRecords)
  }

  def ++(addedRecords: Set[Record]): Schedule = merge(addedRecords)

  /** Merge with another schedule's content. */
  def merge(that: Schedule): Schedule = merge(that.records)

  def ++(that: Schedule): Schedule = merge(that.records)

  /** Adds a person to some topic already on schedule. If the topic is not on schedule, returns the same schedule. */
  def addPersonToExistingTopic(topic: Topic, person: Person): Schedule = updateRecords(_.map {
    case Record(s, t, ps) if t == topic => Record(s, t, ps + person)
    case r => r
  })

  /** Swap two persons. Persons are in couple with there current record, not the target record. */
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

  /** @return Constraints broken by this schedule */
  lazy val brokenConstraints: Set[Constraint] = problem.constraints.filter(_.isBroken(this))

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

  /** A Record is a triplet of slot, topic and assigned persons */
  case class Record(slot: Slot, topic: Topic, persons: Set[Person])

  object Record {
    def fromTuple(tuple: (Slot, Topic, Set[Person])) = Record(tuple._1, tuple._2, tuple._3)

    def fromTuple2(tuple: ((Slot, Topic), Set[Person])) = Record(tuple._1._1, tuple._1._2, tuple._2)

    def apply(slot: Slot, topic: Topic, persons: Person*): Record = apply(slot, topic, persons.toSet)
  }

  /** Empty schedule for a problem */
  def empty(implicit problem: Problem): Schedule = Schedule()

  def apply(schedule: Seq[Record]*)(implicit problem: Problem): Schedule = new Schedule(schedule.flatten.toSet)

  def apply(personsByTopicBySlot: Map[Slot, Map[Topic, Set[Person]]])(implicit problem: Problem) =
    new Schedule(
      personsByTopicBySlot.flatMap {
        case (slot, topicsPersons) => topicsPersons.map {
          case (topic, persons) => Record(slot, topic, persons)
        }
      }.toSet
    )
}
