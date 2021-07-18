package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model._

/** Might be an anti-preference if the person really doesn't want to. */
final case class PersonTopicPreference(
    person: Person,
    topic: Topic,
    reward: Score
) extends Preference.RecordLevel with Preference.Personal {

  override def scoreRecord(record: Record): Score = {
    val checkTopicEquality = record.topic == topic
    val checkPersonsContains = record.personsBitSet.contains(person)
    if (checkTopicEquality && checkPersonsContains) reward
    else Score.Zero
  }
}
