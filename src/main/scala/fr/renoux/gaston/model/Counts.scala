package fr.renoux.gaston.model

import fr.renoux.gaston.util.Count

/** This class stores the counts for each of the three first-level entities in the problem: slots, topics and persons.
  * It is typically passed around implicitly, allowing us to instantiate arrays at the proper size (we prefer arrays to
  * any other collections for performance purposes). */
final case class Counts(
    slots: Int,
    topics: Int,
    persons: Int
)

object Counts {
  val Empty: Counts = Counts(0, 0, 0)

  implicit def fromCounts(implicit slotsCount: Count[Slot], topicsCount: Count[Topic], personsCount: Count[Person]): Counts =
    Counts(slots = slotsCount.value, topics = topicsCount.value, persons = personsCount.value)
}
