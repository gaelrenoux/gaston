package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Constraint, Person, Schedule, Topic}
import fr.renoux.gaston.util.OptionImplicits._

/**
  * A person must be assigned to this topic.
  */
case class PersonTopicObligation(person: Person, topic: Topic) extends Constraint {

  /** Is this constraint respected on the schedule */
  override def isRespected(schedule: Schedule): Boolean =
    schedule.personsPerTopic.get(topic).emptyOrContains(person)

}
