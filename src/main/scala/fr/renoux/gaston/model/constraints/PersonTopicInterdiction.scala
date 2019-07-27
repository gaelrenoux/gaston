package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model._
import fr.renoux.gaston.util.OptionImplicits._

/** A person cannot be assigned to a topic. */
@hardCoded
case class PersonTopicInterdiction(person: Person, topic: Topic) extends Constraint.SlotLevel {

  override def isRespected(schedule: Schedule): Boolean =
    !schedule.personsPerTopic.get(topic).flatContains(person)

  override def isRespectedSlot(schedule: SlotSchedule): Boolean =
    !schedule.personsPerTopic.get(topic).flatContains(person)

}
