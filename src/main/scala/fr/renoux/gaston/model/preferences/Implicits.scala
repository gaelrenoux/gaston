package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.preferences.Preference.Strength
import fr.renoux.gaston.model.{Person, Topic}

/**
  * Created by gael on 07/05/17.
  */
object Implicits {

  implicit class PreferencesOps(wrapped: Iterable[_ <: Preference]) {

    /** From a list of preferences, get all persons with a preference for some topic. */
    def personsWithPreferenceForTopic(topic: Topic): Map[Person, Strength] = wrapped flatMap {
      case PersonTopicPreference(p, t, str) if t == topic => Some(p -> str)
      case _ => None
    } toMap
  }

}
