package fr.renoux.gaston.model

/**
  * Created by gael on 07/05/17.
  */
case class PersonSlotObligation(person: Person, slot: Slot) extends Obligation {

  override def score(solution: Solution): Double =  solution.personsPerSlot map {
    case (s, persons) if s == slot && !persons(person) => ScoringConstants.BrokenMandatoryConstraint
    case _ => ScoringConstants.Zero
  } sum

  override def isRespected(solution: Solution): Boolean = solution.personsPerSlot forall {
    case (s, persons) if s == slot && !persons(person) => false
    case _ => true
  }

}