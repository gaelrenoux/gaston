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

  assert(requiredCount >0, s"$this should have a strictly positive count")

  override def scoreSchedule(schedule: Schedule): Score = {
    var countLeft = requiredCount
    val slotSeqIt = schedule.problem.slotSequences.iterator
    while (countLeft > 0 && slotSeqIt.hasNext) {
      val slotSeq = slotSeqIt.next()
      val foundOne = slotSeq.exists { s => schedule.on(s).unassignedPersons.contains(person) }
      if (foundOne) countLeft -= 1
    }

    reward * countLeft
  }

  override lazy val toLongString: String = s"PersonFreeSlotPreference(${person.toShortString}, $requiredCount, $reward)"

  override lazy val toAbstract: (String, Person.Id, Int, Double) =
    ("PersonFreeSlotPreference", person.id, requiredCount, reward.value)
}

