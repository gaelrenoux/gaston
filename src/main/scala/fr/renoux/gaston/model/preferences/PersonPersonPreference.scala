package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model._

/** Might be an anti-preference if the person really doesn't want to. */
final case class PersonPersonPreference(
    person: Person,
    targetPerson: Person,
    reward: Score
) extends Preference.RecordLevel with Preference.Personal {

  override def scoreRecord(record: Record): Score = {
    if (record.persons.contains(person) && record.persons.contains(targetPerson)) reward
    else Score.Zero
  }

  override lazy val toLongString: String = s"PersonPersonPreference(${person.toShortString}, ${targetPerson.toShortString}, $reward)"

  override lazy val toAbstract: (String, Person.Id, Person.Id, Double) = ("PersonPersonPreference", person.id, targetPerson.id, reward.value)
}
