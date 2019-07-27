package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model._
import fr.renoux.gaston.util.OptionImplicits._

/** Might be an anti-preference if the person really doesn't want to. */
case class PersonTopicPreference(
    person: Person,
    topic: Topic,
    reward: Score
) extends Preference.SlotLevel with Preference.Personal {

  /** Specific implementation, faster than the default */
  override def score(schedule: Schedule): Score = {
    if (schedule.personsPerTopic.get(topic).flatContains(person)) reward
    else Score.Zero
  }

  override def scoreSlot(schedule: SlotSchedule): Score = {
    if (schedule.personsPerTopic.get(topic).flatContains(person)) reward
    else Score.Zero
  }
}
