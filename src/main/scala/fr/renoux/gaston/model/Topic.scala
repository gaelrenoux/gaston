package fr.renoux.gaston.model

/* TODO add cardinality at some point. Persons can want to be on any session of a specific topic, but not on the two. */

/**
  * What we're doing during those slots. Each topic may only appear once in a Schedule.
  */
case class Topic(name: String)
