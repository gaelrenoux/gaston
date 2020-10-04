package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.{Preference, Schedule, Score, Topic}

/** Bonus point (or malus, if the reward is negative) when this topic is present. */
case class TopicDirectPreference(topic: Topic, reward: Score) extends Preference.GlobalLevel {

  /** Specific implementation, faster than the default */
  override def scoreSchedule(schedule: Schedule): Score =
    if (schedule.scheduledTopicsBitSet.contains(topic)) reward else Score.Zero
}

