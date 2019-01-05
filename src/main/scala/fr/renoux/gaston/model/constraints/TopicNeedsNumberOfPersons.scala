package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Person, Schedule, Topic}

/**
  * Min and max number of persons on a topic. Remember it includes everyone, even mandatory persons !
  */
//TODO separate in min and max, to facilitate partial checking
case class TopicNeedsNumberOfPersons(topic: Topic, min: Int, max: Int) extends AbstractConstraint[(Topic, Set[Person])] {

  override protected def elementsChecked(schedule: Schedule): Iterable[(Topic, Set[Person])] = schedule.personsPerTopic

  override protected def check(checked: (Topic, Set[Person])): Boolean = {
    topic != checked._1 || checkBetweenMinMax(checked._2.size)
  }

  private def checkBetweenMinMax(value: Int): Boolean = value >= min && value <= max


  /** If there's a minimum, you can't check a partial solution */
  override val isApplicableToPartialSchedule: Boolean = min == 0

}
