package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model._

/** Might be an anti-preference if the person really doesn't want to. */
final case class PersonTopicPreference(
    person: Person,
    topic: Topic,
    reward: Score
) extends Preference.RecordLevel with Preference.Personal {

  override def scoreRecord(record: Record): Score = {
    if (record.topic == topic && record.personsBitSet.contains(person)) reward
    else Score.Zero
  }

  override def toLongString: String = s"PersonTopicPreference(${person.toShortString}, ${topic.toShortString}, $reward)"
}
