package fr.renoux.gaston.model

/**
  * Created by gael on 07/05/17.
  */
case class PersonSlotInterdiction(person: Person, slot: Slot) extends Interdiction {

  override def evaluate(solution: Solution): Double = solution.personsPerSlot map {
    case (s, persons) if s == slot && persons(person) => ScoringConstants.BrokenMandatoryConstraint
    case _ => ScoringConstants.Zero
  } sum

}