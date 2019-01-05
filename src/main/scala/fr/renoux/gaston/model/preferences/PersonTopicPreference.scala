package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model._

case class PersonTopicPreference(
    person: Person,
    topic: Topic,
    reward: Score
) extends Preference {

  override def score(schedule: Schedule): Score = {
    if (schedule.personsPerTopic(topic)(person)) reward
    else Score.Zero
  }
}