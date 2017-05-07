package fr.renoux.gaston.model

/**
  * Created by gael on 07/05/17.
  */
case class PersonHasSomethingForAllSlots(person: Person, slots: Set[Slot]) extends Constraint with MandatoryConstraint {

  override def score(solution: Solution): Double = solution.personsPerSlot map {
    case (slot, persons) if slots(slot) && !persons(person) => ScoringConstants.BrokenMandatoryConstraint
    case _ => ScoringConstants.Zero
  } sum

  override def isRespected(solution: Solution): Boolean = solution.personsPerSlot forall {
    case (slot, persons) if slots(slot) && !persons(person) => false
    case _ => true
  }

}