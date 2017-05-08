package fr.renoux.gaston.model.constraints

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model._

/**
  * Created by gael on 07/05/17.
  */
case class PersonPresence(person: Person, slot: Slot, presence: Boolean) extends AbstractConstraint[(Slot, Set[Person])] {

  def elementsChecked(schedule: Schedule): Seq[(Slot, Set[Person])] = schedule.personsPerSlot.toSeq

  def check(schedule: Schedule)(checked: (Slot, Set[Person])): Boolean = {
    slot != checked._1 || checked._2(person) == presence
  }

}
