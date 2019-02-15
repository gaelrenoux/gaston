package fr.renoux.gaston.model

/** Basic information about a problem. Not getting into the details of preferences and constraints. */
trait Problem {
  val parallelization: Int
  val slots: Set[Slot]
  val topics: Set[Topic]
  val persons: Set[Person]
  val constraints: Set[Constraint]
  val preferences: Set[Preference]

  val personsCount: Int

  val personsPerSlot: Map[Slot, Set[Person]]

  val personsCountPerSlot: Map[Slot, Int]

  val mandatoryPersonsPerTopic: Map[Topic, Set[Person]]

  val forbiddenPersonsPerTopic: Map[Topic, Set[Person]]

  val minNumberPerTopic: Map[Topic, Int]

  val maxNumberPerTopic: Map[Topic, Int]

  val mandatoryTopicsPerPerson: Map[Person, Set[Topic]]

  def brokenConstraintsIn(solution: Schedule): Set[Constraint]

  def isAcceptablePartial(candidate: Schedule): Boolean

  def isSolvedBy(solution: Schedule): Boolean
}
