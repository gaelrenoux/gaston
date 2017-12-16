package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model._

/**
  * That one person can not be in the same group as one of the others. A group is defined a a set of person sharing a
  * record in the schedule.
  */
case class PersonsIncompatibilityPreference(one: Person, others: Set[Person], strength: Preference.Strength) extends AbstractPreference[Set[Person]] {

  override def elementsChecked(schedule: Schedule): Iterable[Set[Person]] = schedule.records.map(_.persons)

  override def check(checked: Set[Person]): Boolean = {
    !checked.contains(one) || !others.exists(checked.contains)
  }

}
