package fr.renoux.gaston.model

import fr.renoux.gaston.model.utils.Opt

/**
  * Created by gael on 07/05/17.
  */
case class TopicNeedsNumberOfPersons(topic: Topic, min: Opt[Int] = None, max: Opt[Int] = None) extends Constraint with MandatoryConstraint {

  override def score(solution: Solution): Double = solution.personsPerTopic map {
    case (t, persons) if t == topic && !check(persons.size) => ScoringConstants.BrokenMandatoryConstraint
    case _ => ScoringConstants.Zero
  } sum


  override def isRespected(solution: Solution): Boolean = solution.personsPerTopic forall {
    case (t, persons) if t == topic && !check(persons.size) => false
    case _ => true
  }

  private def check(value: Int) = {
    (min forall (value >= _)) && (max forall (value <= _))
  }
}
