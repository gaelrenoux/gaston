package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model._

/**
  * Created by gael on 07/05/17.
  */
case class PersonTopicPreference(
                                  person: Person,
                                  topic: Topic,
                                  reward: Score) extends Preference {

  override def score(schedule: Schedule): Score = {
    val topicPersons = schedule.personsPerTopic(topic)
    if (topicPersons(person)) person.weight * reward
    else Score.Zero
  }
}