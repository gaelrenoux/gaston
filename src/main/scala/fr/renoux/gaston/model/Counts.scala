package fr.renoux.gaston.model

import fr.renoux.gaston.util.Count

case class Counts(
    slots: Int,
    topics: Int,
    persons: Int
) {
  object implicits {
    implicit val implicitSlotsCount: Count[Slot] = Count[Slot](slots)
    implicit val implicitTopicsCount: Count[Topic] = Count[Topic](topics)
    implicit val implicitPersonsCount: Count[Person] = Count[Person](persons)
  }
}
