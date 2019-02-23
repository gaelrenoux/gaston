package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model._

case class PersonTopicPreference(
    person: Person,
    topic: Topic,
    reward: Score
) extends Preference.SlotLevel {

  /** Specific implementation, faster than the default */
  override def score(schedule: Schedule): Score = {
    if (schedule.personsPerTopic(topic)(person)) reward
    else Score.Zero
  }

  override def scoreSlot(schedule: SlotSchedule): Score = {
    if (schedule.personsPerTopic(topic)(person)) reward
    else Score.Zero
  }
}