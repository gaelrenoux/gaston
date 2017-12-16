package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Person, Schedule, Topic}
import fr.renoux.gaston.util.Opt

/**
  * Min and max number of persons on a topic.
  */
case class TopicNeedsNumberOfPersons(topic: Topic, min: Opt[Int] = None, max: Opt[Int] = None) extends AbstractConstraint[(Topic, Set[Person])] {

  override protected def elementsChecked(schedule: Schedule): Iterable[(Topic, Set[Person])] = schedule.personsPerTopic

  override protected def check(checked: (Topic, Set[Person])): Boolean = {
    topic != checked._1 || checkBetweenMinMax(checked._2.size)
  }

  private def checkBetweenMinMax(value: Int) = {
    (min forall (value >= _)) && (max forall (value <= _))
  }

  /** If there's a minimum, you can't check a partial solution */
  override val isApplicableToPartialSolution: Boolean = min.isEmpty

}
