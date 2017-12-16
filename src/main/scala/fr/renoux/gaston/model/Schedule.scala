package fr.renoux.gaston.model

/**
  * What we're trying and testing and looking for a good one.
  */
case class Schedule(
                     records: Set[Schedule.Record]
                   ) {

  //private lazy val slots = triplets map (_._1)
  lazy val personsPerSlot: Map[Slot, Set[Person]] = records groupBy (_.slot) mapValues { x => x flatMap (_.persons) }
  lazy val personsPerTopic: Map[Topic, Set[Person]] = records groupBy (_.topic) mapValues { x => x flatMap (_.persons) }
  lazy val topicsPerSlot: Map[Slot, Set[Topic]] = records groupBy (_.slot) mapValues { x => x map (_.topic) }
  lazy val countPersonsPerTopic: Map[Topic, Int] = personsPerTopic mapValues (_.size)

  /** Merge more triplets into this schedule. */
  def merge(addedRecords: Set[Schedule.Record]): Schedule = {
    val cumulatedRecords = records ++ addedRecords
    val mergedMap = cumulatedRecords groupBy (t => (t.slot, t.topic)) mapValues (_.flatMap(_.persons))
    val mergedRecords = mergedMap.toSet map Schedule.Record.fromTuple2
    Schedule(mergedRecords)
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
