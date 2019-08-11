package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model
import fr.renoux.gaston.model.{Preference, Schedule, Score, Topic}

/** This topic must be present. */
case class TopicForced(topic: Topic, reward: Score = Preference.NecessaryPreferenceScore)
  extends Preference.Anti {

  /** Specific implementation, faster than the default */
  override def score(schedule: Schedule): Score =
    if (schedule.topics.contains(topic)) model.Score.Zero else reward
}

