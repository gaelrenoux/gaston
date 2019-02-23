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
) extends Preference.SlotLevel with Preference.Anti {

  /** Specific implementation, faster than the default */
  override def score(schedule: Schedule): Score = {
    schedule.personGroups.filter(_.contains(person)).map { groupWithPerson =>
      val count = group.intersect(groupWithPerson).size
      if (count == 0) Score.Zero else reward * count
    }.sum
  }

  override def scoreSlot(schedule: SlotSchedule): Score = {
    schedule.personGroups.filter(_.contains(person)).map { groupWithPerson =>
      val count = group.intersect(groupWithPerson).size
      if (count == 0) Score.Zero else reward * count
    }.sum
  }

}
