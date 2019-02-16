package fr.renoux.gaston.model

import fr.renoux.gaston.util.CollectionImplicits._

/**
  * A schedule is a set of triplets matching topics to slots, and having people attached to the topics.
  * What we're trying and testing and looking for a good one.
  */
case class Schedule(
    records: Set[Schedule.Record]
)(implicit
    val problem: Problem
) {

  import fr.renoux.gaston.model.Schedule._

  lazy val slots: Set[Slot] = records.map(_.slot)
  lazy val personsPerSlot: Map[Slot, Set[Person]] = records.groupBy(_.slot).mapValuesStrict { x => x.flatMap(_.persons) }.withDefaultValue(Set())
  lazy val personsPerTopic: Map[Topic, Set[Person]] = records.groupBy(_.topic).mapValuesStrict { x => x.flatMap(_.persons) }.withDefaultValue(Set())
  lazy val topicsPerSlot: Map[Slot, Set[Topic]] = records.groupBy(_.slot).mapValuesStrict { x => x.map(_.topic) }.withDefaultValue(Set())
  lazy val countPersonsPerTopic: Map[Topic, Int] = personsPerTopic.mapValuesStrict(_.size).withDefaultValue(0)

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

  /** Merge more triplets into this schedule. */
  def merge(that: Schedule): Schedule = merge(that.records)

  def ++(that: Schedule): Schedule = merge(that.records)

  /** Adds a person to some topic already on schedule. Has no effect if the topic is not on schedule. */
  def addPersonToTopic(person: Person, topic: Topic): Schedule = copy(records = records.map {
    case Record(s, t, ps) if t == topic => Record(s, t, ps + person)
    case r => r
  }
  )

  /** The schedule makes sense. No person on multiple topics at the same time. */
  lazy val isSound: Boolean = {
    records.groupBy(_.slot).values.forall { records =>
      val persons = records.toSeq.flatMap(_.persons.toSeq) //toSeq to keep duplicates, we're looking for them
      persons.size == persons.toSet.size
    }
  }

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
