package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Person, Schedule, Topic}

/**
  * Min and max number of persons on a topic.
  */
case class TopicNeedsNumberOfPersons(topic: Topic, min: Int, max: Int) extends AbstractConstraint[(Topic, Set[Person])] {

  override protected def elementsChecked(schedule: Schedule): Iterable[(Topic, Set[Person])] = schedule.personsPerTopic

  override protected def check(checked: (Topic, Set[Person])): Boolean = {
    topic != checked._1 || checkBetweenMinMax(checked._2.size)
  }

  private def checkBetweenMinMax(value: Int) = value >= min && value <= max


  /** If there's a minimum, you can't check a partial solution */
  override val isApplicableToPartialSolution: Boolean = min == 0

}
