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
) extends Preference.Anti {

  def score(schedule: Schedule): Score = {
    schedule.records.foldLeft(Score.Zero) {
      case (sum, r) if r.persons.contains(person) =>
        val intersection = r.persons.intersect(group).size
        if (intersection == 0) sum else sum + (reward * intersection)
      case (sum, _) => sum
    }
  }

}
