package fr.renoux.gaston.model

/**
  * Created by gael on 07/05/17.
  */
case class Schedule(
                     triplets: Set[(Slot, Topic, Set[Person])]
                   ) {

  lazy val personsPerSlot: Map[Slot, Set[Person]] = triplets groupBy (_._1) mapValues { x => x flatMap (_._3) }
  lazy val personsPerTopic: Map[Topic, Set[Person]] = triplets groupBy (_._2) mapValues { x => x flatMap (_._3) }

}

object Schedule {
  def apply(schedule: (Slot, Topic, Set[Person])*): Schedule = new Schedule(schedule.toSet)
}
