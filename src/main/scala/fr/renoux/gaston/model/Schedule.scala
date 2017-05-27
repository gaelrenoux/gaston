package fr.renoux.gaston.model

/**
  * What we're trying and testing and looking for a good one.
  */
case class Schedule(
                     triplets: Set[(Slot, Topic, Set[Person])]
                   ) {

  lazy val personsPerSlot: Map[Slot, Set[Person]] = triplets groupBy (_._1) mapValues { x => x flatMap (_._3) }
  lazy val personsPerTopic: Map[Topic, Set[Person]] = triplets groupBy (_._2) mapValues { x => x flatMap (_._3) }
  lazy val topicsPerSlot: Map[Slot, Set[Topic]] = triplets groupBy (_._1) mapValues { x => x map (_._2) }
  lazy val countPersonsPerTopic: Map[Topic, Int] = personsPerTopic mapValues (_.size)

  def merge(addedTriplets: Set[(Slot, Topic, Set[Person])]): Schedule = {
    val cumulatedTriplets = triplets ++ addedTriplets
    val mergedMap = cumulatedTriplets groupBy(t => (t._1, t._2)) mapValues(_.flatMap(_._3))
    val mergedTriplets = mergedMap.toSet map { c: ((Slot, Topic), Set[Person]) => (c._1._1, c._1._2, c._2) }
    copy(triplets = mergedTriplets)
  }

}

object Schedule {
  def apply(schedule: (Slot, Topic, Set[Person])*): Schedule = new Schedule(schedule.toSet)
}
