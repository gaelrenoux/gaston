package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.{Person, Preference, Schedule, Score, Topic}

/**
 * This person wants a minimum of X free slots in its schedule. If >1, those free slots must be on different slot-sequences.
 *
 * This is normally used as a soft-constraint (very high negative score, in order to make sure it's always respected).
 * It's not a Preference.Personal, because those are always record-level and this one is by necessity global-level.
 */
final case class PersonFreeSlotPreference(person: Person, requiredCount: Int, reward: Score = Preference.NecessaryPreferenceScore)
  extends Preference.GlobalLevel with Preference.Anti {

  assert(requiredCount > 0, s"$this should have a strictly positive count")

  override def scoreSchedule(schedule: Schedule): Score = {
    val count = schedule.countSlotCyclesWithUnassignedByPerson(person)
    val diff = requiredCount - count
    if (diff <= 0) Score.Zero
    else reward * diff
  }

  override lazy val toLongString: String = s"PersonFreeSlotPreference(${person.toShortString}, $requiredCount, $reward)"

  override lazy val toAbstract: (String, Person.Id, Int, Double) =
    ("PersonFreeSlotPreference", person.id, requiredCount, reward.value)
}

