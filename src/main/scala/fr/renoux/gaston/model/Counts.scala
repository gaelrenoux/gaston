package fr.renoux.gaston.model

/** This class stores the counts for each of the three first-level entities in the problem: slots, topics and persons.
  * It is typically passed around implicitly, allowing us to instantiate arrays at the proper size (we prefer arrays to
  * any other collections for performance purposes). */
case class Counts(
    slots: Int,
    topics: Int,
    persons: Int
)
