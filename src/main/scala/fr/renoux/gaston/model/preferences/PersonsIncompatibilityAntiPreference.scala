package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model._

/**
  * That one person can not be in the same group as one of the others. A group is defined a a set of person sharing a
  * record in the schedule. The reward here is negative, as we count the number of such occurences, and counts once for
  * each couple violating the preference.
  */
case class PersonsIncompatibilityAntiPreference(one: Person, others: Set[Person], reward: Score) extends AbstractPreference[Set[Person]] {

  assert (reward.value <= 0)

  override protected def elementsScored(schedule: Schedule): Iterable[Set[Person]] =
    schedule.records.toSeq.map(_.persons) //toSeq needed: we do not want to deduplicate !

  override protected def score(scored: Set[Person]): Score = {
    if (!scored.contains(one)) Score.Zero
    else others.intersect(scored) map { p =>
      Weight.combine(one.weight, p.weight) * reward
    } sum
  }

}
