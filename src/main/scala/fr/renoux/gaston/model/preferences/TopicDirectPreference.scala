package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.{Preference, Schedule, FlatScore, Topic}

/** Bonus point (or malus, if the reward is negative) when this topic is present. */
final case class TopicDirectPreference(topic: Topic, reward: FlatScore) extends Preference.GlobalLevel with Preference.Impersonal {

  /** Doesn't matter at all how persons are assigned. */
  override def personsMatter: Boolean = false

  /** Specific implementation, faster than the default */
  override def scoreSchedule(schedule: Schedule): FlatScore =
    if (schedule.scheduledTopicsBitSet.contains(topic)) reward else FlatScore.Zero

  override lazy val toLongString: String = s"TopicDirectPreference(${topic.toShortString}, $reward)"

  override lazy val toAbstract: (String, Int, Double) = ("TopicDirectPreference",  topic.id, reward.value)
}

