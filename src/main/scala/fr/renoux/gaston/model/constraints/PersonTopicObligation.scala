package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Person, Schedule, Topic}

/**
  * A person must be assigned to ant instance of this topic.
  */
case class PersonTopicObligation(person: Person, topic: Topic) extends AbstractConstraint[(Topic, Set[Person])] {

  def elementsChecked(schedule: Schedule): Seq[(Topic, Set[Person])] = schedule.personsPerTopic.toSeq

  def check(schedule: Schedule)(checked: (Topic, Set[Person])): Boolean = {
    topic != checked._1 || checked._2(person)
  }

}
