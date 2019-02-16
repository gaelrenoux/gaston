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
) extends AbstractPreference[Set[Person]] with Preference.Anti {

  /** Elements to score are the group of persons containing the current person, independently of the slots and topics */
  override protected def elementsScored(schedule: Schedule): Iterable[Set[Person]] =
  //toSeq is needed: we do not want to deduplicate identical groups!
    schedule.records.filter(_.persons(person)).toSeq.map(_.persons)

  /** To score a group: count the number of unwanted persons */
  override protected def score(scored: Set[Person]): Score = {
    val count = group.intersect(scored).size
    if (count == 0) Score.Zero
    else reward * count
  }

}
