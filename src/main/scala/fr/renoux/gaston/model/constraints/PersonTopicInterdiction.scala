package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Person, Schedule, Topic}

/**
  * A person cannot be assigned to a topic.
  */
case class PersonTopicInterdiction(person: Person, topic: Topic) extends AbstractConstraint[(Topic, Set[Person])] {

  override def elementsChecked(schedule: Schedule): Iterable[(Topic, Set[Person])] = schedule.personsPerTopic

  override def check(checked: (Topic, Set[Person])): Boolean = {
    topic != checked._1 || !checked._2(person)
  }
}
