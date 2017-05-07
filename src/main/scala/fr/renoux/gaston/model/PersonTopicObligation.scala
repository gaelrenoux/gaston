package fr.renoux.gaston.model

import com.typesafe.scalalogging.Logger

/**
  * Created by gael on 07/05/17.
  */
case class PersonTopicObligation(person: Person, topic: Topic) extends Obligation {

  val logger = Logger[PersonTopicObligation]

  override def score(solution: Solution): Double = solution.personsPerTopic map {
    case (t, persons) if t == topic && !persons(person) => ScoringConstants.BrokenMandatoryConstraint
    case _ => ScoringConstants.Zero
  } sum

  override def isRespected(solution: Solution): Boolean = solution.personsPerTopic forall {
    case (t, persons) if t == topic && !persons(person) => false
    case _ => true
  }

}
