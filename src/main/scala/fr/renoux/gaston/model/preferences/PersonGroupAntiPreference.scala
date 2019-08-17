package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model._

/**
  * That one person doesn't want to share a schedule record with any person from that group. The reward here is
  * negative, as we count the number of such occurrences. Note that it does not apply on the members of the group, who
  * mah have no anti-preference towards the person.
  */
case class PersonGroupAntiPreference(
    person: Person,
    group: Set[Person],
    reward: Score
) extends Preference.RecordLevel with Preference.Anti with Preference.Personal {

  override def scoreRecord(record: Record): Score = {
    if (record.persons.contains(person)) {
      val count = group.intersect(record.persons).size
      if (count == 0) Score.Zero else reward * count
    } else Score.Zero
  }

}
