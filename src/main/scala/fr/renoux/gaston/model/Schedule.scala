package fr.renoux.gaston.model

/**
  * What we're trying and testing and looking for a good one.
  */
case class Schedule(
                     records: Set[Schedule.Record]
                   ) {

  import fr.renoux.gaston.model.Schedule._

  //private lazy val slots = triplets map (_._1)
  lazy val personsPerSlot: Map[Slot, Set[Person]] = records groupBy (_.slot) mapValues { x => x flatMap (_.persons) }
  lazy val personsPerTopic: Map[Topic, Set[Person]] = records groupBy (_.topic) mapValues { x => x flatMap (_.persons) }
  lazy val topicsPerSlot: Map[Slot, Set[Topic]] = records groupBy (_.slot) mapValues { x => x map (_.topic) }
  lazy val countPersonsPerTopic: Map[Topic, Int] = personsPerTopic mapValues (_.size)

  /** Merge more triplets into this schedule. */
  def merge(addedRecords: Set[Record]): Schedule = {
    val cumulatedRecords = records ++ addedRecords
    val mergedMap = cumulatedRecords groupBy (t => (t.slot, t.topic)) mapValues (_.flatMap(_.persons))
    val mergedRecords = mergedMap.toSet map Record.fromTuple2
    Schedule(mergedRecords)
  }

  def addPersonToTopic(person: Person, topic: Topic) = Schedule {
    if (topic.name == "gamma") {
      val count = records.find(_.topic.name == "gamma").map(_.persons.size).getOrElse(0)
      if (count > 11)
        throw new IllegalArgumentException
    }

    records map {
      case Record(s, t, ps) if t == topic => Record(s, t, ps + person)
      case r => r
    }
  }

  lazy val toFormattedString: String = {
    val builder = new StringBuilder("Schedule:\n")
    val personsPerTopicPerSlot = records.groupBy(_.slot.name).mapValues(_.groupBy(_.topic.name).mapValues(_.flatMap(_.persons).map(_.name)))
    val orderedPersonsPerTopicPerSlot = personsPerTopicPerSlot.mapValues(_.mapValues(_.toSeq.sorted).toSeq.sortBy(_._1)).toSeq.sortBy(_._1)

    for ((slot, personsPerTopic) <- orderedPersonsPerTopicPerSlot) {
      builder.append("  ").append(slot).append(": \n")
      for ((topic, persons) <- personsPerTopic) {
        builder.append("    ").append(topic).append(": ").append(persons.mkString("", ", ", "\n"))
      }
    }

    builder.toString
  }

}

object Schedule {
  case class Record(slot: Slot, topic: Topic, persons: Set[Person])

  object Record {
    def fromTuple(tuple: (Slot, Topic, Set[Person])) = Record(tuple._1, tuple._2, tuple._3)
    def fromTuple2(tuple: ((Slot, Topic), Set[Person])) = Record(tuple._1._1, tuple._1._2, tuple._2)
  }

  def apply(schedule: Record*): Schedule = new Schedule(schedule.toSet)
}
