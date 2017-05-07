package fr.renoux.gaston.model

/**
  * Created by gael on 07/05/17.
  */
case class PersonHasSomethingForAllSlots(person: Person, slots: Set[Slot]) extends Constraint {
  val isMandatory = true

  def evaluate(solution: Solution): Double = solution.personsPerSlot map {
    case (slot, persons) if slots(slot) && !persons(person) => ScoringConstants.BrokenMandatoryConstraint
    case _ => ScoringConstants.Zero
  } sum

}