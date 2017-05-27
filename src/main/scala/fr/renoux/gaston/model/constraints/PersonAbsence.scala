package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model._

/**
  * A person is missing on some spot.
  */
case class PersonAbsence(person: Person, slot: Slot) extends AbstractConstraint[(Slot, Set[Person])] {

  def elementsChecked(schedule: Schedule): Seq[(Slot, Set[Person])] = schedule.personsPerSlot.toSeq

  def check(schedule: Schedule)(checked: (Slot, Set[Person])): Boolean = {
    slot != checked._1 || !checked._2(person)
  }

}
