package fr.renoux.gaston.model

import fr.renoux.gaston.model.Preference.Strength

/**
  * Created by gael on 07/05/17.
  */
object Implicits {

  implicit class ConstraintsOps(wrapped: Iterable[_<:Constraint]) {
    def personsMandatoryForTopic(topic: Topic): Set[Person] = wrapped flatMap {
      case PersonTopicObligation(p, t) => Some(p)
      case _ => None
    } toSet

    def personsWithPreferenceForTopic(topic: Topic): Map[Person, Strength] = wrapped flatMap {
      case PersonTopicPreference(p, t, str) => Some(p -> str)
      case _ => None
    } toMap
  }

}
