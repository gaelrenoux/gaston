package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model._

/**
  * A person is missing on some slot.
  */
case class PersonAbsence(person: Person, slot: Slot) extends AbstractConstraint[(Slot, Set[Person])] {

  override protected def elementsChecked(schedule: Schedule): Iterable[(Slot, Set[Person])] = schedule.personsPerSlot

  override protected def check(checked: (Slot, Set[Person])): Boolean = {
    slot != checked._1 || !checked._2(person)
  }

}
