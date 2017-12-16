package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model._

/**
  * Created by gael on 07/05/17.
  */
case class PersonTopicPreference(
                                  person: Person,
                                  topic: Topic,
                                  strength: Preference.Strength) extends AbstractPreference[(Topic, Set[Person])] {

  override def elementsChecked(schedule: Schedule): Iterable[(Topic, Set[Person])] = schedule.personsPerTopic

  override def check(checked: (Topic, Set[Person])): Boolean = {
    checked._1 == topic && checked._2(person)
  }
}