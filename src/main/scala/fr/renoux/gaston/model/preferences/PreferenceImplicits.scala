package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.{Person, Score, Topic}


object PreferenceImplicits {

  implicit class PreferencesOps(wrapped: Iterable[_ <: Preference]) {

    /** From a list of preferences, get all persons with a preference for some topic (and the reward of that preference). */
    def personsWithPreferenceForTopic(topic: Topic): Map[Person, Score] = wrapped.collect {
      case PersonTopicPreference(p, t, str) if t == topic => p -> str
    }.toMap
  }

}
