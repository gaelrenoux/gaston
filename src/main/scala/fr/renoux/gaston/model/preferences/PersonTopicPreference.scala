package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model._

/** Might be an anti-preference if the person really doesn't want to. */
final case class PersonTopicPreference(
    person: Person,
    topic: Topic,
    reward: FlatScore
) extends Preference.RecordLevel with Preference.Personal {

  override def scoreRecord(record: Record): FlatScore = {
    if (record.topic == topic && record.personsBitSet.contains(person)) reward
    else FlatScore.Zero
  }

  override lazy val toLongString: String = s"PersonTopicPreference(${person.toShortString}, ${topic.toShortString}, $reward)"

  override lazy val toAbstract: (String, Person.Id, Int, Double) = ("PersonTopicPreference", person.id, topic.id, reward.value)
}
