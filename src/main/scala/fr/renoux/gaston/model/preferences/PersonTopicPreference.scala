package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.*

/** Might be an anti-preference if the person really doesn't want to. */
final case class PersonTopicPreference(
    person: Person,
    topic: Topic,
    reward: Score
) extends Preference.RecordLevel with Preference.Personal {

  override def scoreRecord(record: Record): Score = {
    if (record.topic == topic && record.personsArraySet.contains(person)) reward
    else Score.Zero
  }

  override lazy val toLongString: String = s"PersonTopicPreference(${person.toShortString}, ${topic.toShortString}, $reward)"

  override lazy val toAbstract: (String, Person.Id, Int, Double) = ("PersonTopicPreference", person.id, topic.id, reward.value)
}
