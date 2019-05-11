package fr.renoux.gaston.model

import fr.renoux.gaston.util.CollectionImplicits._

/** A view on the schedule, on one specific slot */
case class SlotSchedule(
    schedule: Schedule,
    slot: Slot
) {
  val problem: Problem = schedule.problem
  lazy val records: Set[Record] = schedule.records.filter(_.slot == slot)
  lazy val topics: Set[Topic] = schedule.topicsPerSlot(slot)
  lazy val persons: Set[Person] = records.flatMap(_.persons)
  lazy val personsPerTopic: Map[Topic, Set[Person]] = records.groupBy(_.topic).mapValuesStrict(_.flatMap(_.persons))
  lazy val countPersonsPerTopic: Map[Topic, Int] = personsPerTopic.mapValuesStrict(_.size)
  lazy val personGroups: Iterable[Set[Person]] = records.view.map(_.persons).toList //not a Set: we do not want to deduplicate identical groups!
  lazy val mandatory: Set[Person] = schedule.mandatoryPersonsOnSlot(slot)
  lazy val minPersons: Int = schedule.minPersonsOnSlot(slot)
  lazy val maxPersons: Int = schedule.maxPersonsOnSlot(slot)

  /** Topics that cannot be added on this slot, because of the slot itself */
  lazy val hardIncompatibleTopics: Set[Topic] = problem.incompatibleTopicsPerSlot(slot)

  /** Topics that cannot be added on this slot, because of the slot or other topics */
  lazy val currentIncompatibleTopics: Set[Topic] = hardIncompatibleTopics ++ topics.flatMap(problem.incompatibleTopicsPerTopic)

  /** Get the Persons for a specific Topic */
  def on(t: Topic): Set[Person] = personsPerTopic.getOrElse(t, Set())

}
