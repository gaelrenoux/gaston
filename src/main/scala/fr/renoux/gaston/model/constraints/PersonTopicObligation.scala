package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Person, Schedule, Topic}

/**
  * A person must be assigned to an instance of this topic.
  */
case class PersonTopicObligation(person: Person, topic: Topic) extends AbstractConstraint[(Topic, Set[Person])] {

  override protected def elementsChecked(schedule: Schedule): Iterable[(Topic, Set[Person])] = schedule.personsPerTopic

  override protected def check(checked: (Topic, Set[Person])): Boolean = {
    topic != checked._1 || checked._2(person)
  }

}
