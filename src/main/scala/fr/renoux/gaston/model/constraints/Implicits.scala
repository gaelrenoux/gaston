package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Person, Topic}

/**
  * Created by gael on 07/05/17.
  */
object Implicits {

  implicit class ConstraintsOps(wrapped: Iterable[_ <: Constraint]) {
    def personsMandatoryForTopic(topic: Topic): Set[Person] = wrapped flatMap {
      case PersonTopicObligation(p, t) => Some(p)
      case _ => None
    } toSet
  }

}
