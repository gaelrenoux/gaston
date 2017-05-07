package fr.renoux.gaston.model

/**
  * Created by gael on 07/05/17.
  */
case class PersonTopicPreference(
                                  person: Person,
                                  topic: Topic,
                                  value: Preference.Strength) extends Preference {

  override def evaluate(solution: Solution): Double = solution.personsPerTopic map {
    case (t, persons) if t == topic && persons(person) => value.score
    case _ => ScoringConstants.Zero
  } sum
}