package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model._
import fr.renoux.gaston.util.OptionImplicits._

/**
  * A person is missing on some slot.
  */
case class PersonAbsence(person: Person, slot: Slot) extends Constraint.SlotLevel {

  override def isRespected(schedule: Schedule): Boolean =
    !schedule.personsPerSlot.get(slot).flatContains(person)

  override def isRespectedSlot(schedule: SlotSchedule): Boolean =
    schedule.slot != slot || !schedule.persons.contains(person)

}
