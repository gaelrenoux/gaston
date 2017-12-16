package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.{Person, Topic}

/**
  * Created by gael on 07/05/17.
  */
object Implicits {

  implicit class ConstraintsOps(wrapped: Iterable[_ <: Constraint]) {
    /** From a list of constraints, get all persons mandatory for some topic. */
    def personsMandatoryForTopic(topic: Topic): Set[Person] = wrapped collect {
      case PersonTopicObligation(p, t) if t == topic => p
    } toSet
  }

}
