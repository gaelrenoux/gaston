package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.preferences.Preference.Strength
import fr.renoux.gaston.model.{Person, Topic}

/**
  * Created by gael on 07/05/17.
  */
object Implicits {

  implicit class PreferencesOps(wrapped: Iterable[_ <: Preference]) {

    def personsWithPreferenceForTopic(topic: Topic): Map[Person, Strength] = wrapped flatMap {
      case PersonTopicPreference(p, t, str) => Some(p -> str)
      case _ => None
    } toMap
  }

}
