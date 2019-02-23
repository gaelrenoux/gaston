package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model._
import fr.renoux.gaston.util.OptionImplicits._

/**
  * A person must be assigned to this topic.
  */
case class PersonTopicObligation(person: Person, topic: Topic) extends Constraint.SlotLevel {

  override def isRespected(schedule: Schedule): Boolean =
    schedule.personsPerTopic.get(topic).emptyOrContains(person)

  override def isRespectedSlot(schedule: SlotSchedule): Boolean =
    schedule.personsPerTopic.get(topic).emptyOrContains(person)

}
