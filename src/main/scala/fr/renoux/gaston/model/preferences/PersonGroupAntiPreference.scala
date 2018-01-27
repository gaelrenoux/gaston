package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model._

/**
  * That one person doesn't want to share a schedule record with any person from that group. The reward here is
  * negative, as we count the number of such occurrences.
  */
case class PersonGroupAntiPreference(person: Person, group: Set[Person], reward: Score) extends AbstractPreference[Set[Person]] {

  assert(reward.value <= 0, "AntiPreference has a positive reward !")

  override protected def elementsScored(schedule: Schedule): Iterable[Set[Person]] =
    schedule.records.filter(_.persons(person)).toSeq.map(_.persons) //toSeq needed: we do not want to deduplicate !

  override protected def score(scored: Set[Person]): Score = {
    val count = group.intersect(scored).size
    if (count == 0) Score.Zero
    else reward * count
  }

}
