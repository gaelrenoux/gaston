package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Constraint, Schedule, SlotSchedule, Topic}


/**
  * Min and max number of persons on a topic. Remember it includes everyone, even mandatory persons !
  */
//TODO separate in min and max, to facilitate partial checking
case class TopicNeedsNumberOfPersons(topic: Topic, min: Int, max: Int) extends Constraint.SlotLevel {

  /** Is this constraint respected on the schedule */
  override def isRespected(schedule: Schedule): Boolean =
    schedule.countPersonsPerTopic.get(topic).forall(checkBetweenMinMax)

  override def isRespectedSlot(schedule: SlotSchedule): Boolean = {
    schedule.countPersonsPerTopic.get(topic).forall(checkBetweenMinMax)
  }

  private def checkBetweenMinMax(value: Int): Boolean = value >= min && value <= max


  /** If there's a minimum, you can't check a partial solution */
  override val isApplicableToPartialSchedule: Boolean = min == 0

}
