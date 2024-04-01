package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.{Preference, Schedule, Score, Topic}

/** Bonus point (or malus, if the reward is negative) when this topic is present. */
final case class TopicDirectPreference(topic: Topic, reward: Score) extends Preference.GlobalLevel with Preference.Impersonal {

  /** Doesn't matter at all how persons are assigned. */
  override def personsMatter: Boolean = false

  /** Specific implementation, faster than the default */
  override def scoreSchedule(schedule: Schedule): Score =
    if (schedule.scheduledTopicsBitSet.contains(topic)) reward else Score.Zero

  override def toLongString: String = s"TopicDirectPreference(${topic.toShortString}, $reward)"

}

