package fr.renoux.gaston.model

/**
  * Created by gael on 07/05/17.
  */
case class PersonTopicInterdiction(person: Person, topic: Topic) extends Interdiction {

  override def evaluate(solution: Solution): Double = solution.personsPerTopic map {
    case (t, persons) if t == topic && persons(person) => ScoringConstants.BrokenMandatoryConstraint
    case _ => ScoringConstants.Zero
  } sum
}
