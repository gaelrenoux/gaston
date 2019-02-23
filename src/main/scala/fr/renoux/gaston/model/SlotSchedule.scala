package fr.renoux.gaston.model

import fr.renoux.gaston.util.CollectionImplicits._

/** A view on the schedule, on one specific slot */
case class SlotSchedule(
    schedule: Schedule,
    slot: Slot
) {
  lazy val records: Set[Schedule.Record] = schedule.records.filter(_.slot == slot)
  lazy val topics: Set[Topic] = schedule.topicsPerSlot(slot)
  lazy val persons: Set[Person] = records.flatMap(_.persons)
  lazy val personsPerTopic: Map[Topic, Set[Person]] = records.groupBy(_.topic).mapValuesStrict(_.flatMap(_.persons)).withDefaultValue(Set())
  lazy val countPersonsPerTopic: Map[Topic, Int] = personsPerTopic.mapValuesStrict(_.size).withDefaultValue(0)
  lazy val personGroups: Iterable[Set[Person]] = records.toSeq.map(_.persons) //not a Set: we do not want to deduplicate identical groups!

  /** Get the Persons for a specific Topic */
  def on(t: Topic): Set[Person] = personsPerTopic.getOrElse(t, Set())

}
