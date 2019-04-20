package fr.renoux.gaston.model.problem

import fr.renoux.gaston.model.{Preference, _}
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.util.CanGroupToMap._
import fr.renoux.gaston.util.CollectionImplicits._

/** A problem to solve. A schedule solves a problem. */
class ProblemImpl(
    val slots: Set[Slot],
    val topics: Set[Topic],
    val persons: Set[Person],
    val constraints: Set[Constraint],
    val preferences: Set[Preference]
) extends Problem {

  lazy val personsCount: Int = persons.size

  /** Max number of topics on a single slot */
  override val maxTopicCountPerSlot: Map[Slot, Int] = constraints.collect {
    case SlotMaxTopicCount(slot, count) => slot -> count
  }.toMap.withDefaultValue(Int.MaxValue)

  /** For each topic, which persons are mandatory */
  lazy val mandatoryPersonsPerTopic: Map[Topic, Set[Person]] = {

    val topicsWithMandatoryPersons: Map[Topic, Set[Person]] = constraints.collect {
      case PersonTopicObligation(person, topic) => topic -> person
    }.groupBy(_._1).mapValuesStrict(_.map(_._2))

    topics.map(_ -> Set[Person]()).toMap ++ topicsWithMandatoryPersons
  }

  lazy val mandatoryTopicsPerPerson: Map[Person, Set[Topic]] = {

    val personsWithMandatoryTopics: Map[Person, Set[Topic]] = constraints.collect {
      case PersonTopicObligation(person, topic) => person -> topic
    }.groupBy(_._1).mapValuesStrict(_.map(_._2))

    persons.map(_ -> Set[Topic]()).toMap ++ personsWithMandatoryTopics
  }

  /** For each topic, which persons are forbidden */
  lazy val forbiddenPersonsPerTopic: Map[Topic, Set[Person]] = {

    val topicsWithForbiddenPersons: Map[Topic, Set[Person]] = constraints.collect {
      case PersonTopicInterdiction(person, topic) => topic -> person
    }.groupBy(_._1).mapValuesStrict(_.map(_._2))

    topics.map(_ -> Set[Person]()).toMap ++ topicsWithForbiddenPersons
  }

  /** Indicates wether a person is available on a slot or not. */
  lazy val personSlotsPossibilities: Set[(Person, Slot)] = {
    val absences = constraints.collect { case PersonAbsence(p, s) => (p, s) }
    for {p <- persons; s <- slots if !absences((p, s))} yield (p, s)
  }

  /** For each persons, its available slots */
  lazy val slotsPerPerson: Map[Person, Set[Slot]] = personSlotsPossibilities.groupToMap

  /** For each slot, the available persons */
  lazy val personsPerSlot: Map[Slot, Set[Person]] = personSlotsPossibilities.map(_.swap).groupToMap

  /** For each slot, the number of available persons */
  lazy val personsCountPerSlot: Map[Slot, Int] = personsPerSlot.mapValuesStrict(_.size)

  /** For each topic, the topics that cannot be held in the same slot because of some constraints (like the same persons
    * are mandatory). */
  lazy val incompatibleTopicsPerTopic: Map[Topic, Set[Topic]] = {
    val couples = for {
      topic1 <- topics
      topic2 <- topics
      if mandatoryPersonsPerTopic(topic1).intersect(mandatoryPersonsPerTopic(topic2)).nonEmpty
    } yield (topic1, topic2)

    couples.groupToMap
  }

  /** For each slot, the topics that cannot be held in that slot because of some constraints (like some mandatory person
    * is missing). */
  lazy val incompatibleTopicsPerSlot: Map[Slot, Set[Topic]] = {
    val couples = for {
      slot <- slots
      topic <- topics
      if mandatoryPersonsPerTopic(topic).exists(!personsPerSlot(slot).contains(_))
      //TODO add topic explicitely forbidden on the slot
    } yield (slot, topic)
    couples.groupToMap
  }

  /** For each slots, the topics that must happen in that slot. Handles only topics with just one possible slot. */
  lazy val forcedTopicsPerSlot: Map[Slot, Set[Topic]] =
    constraints.collect {
      case TopicForcedSlot(topic, slotSet) if slotSet.size == 1 => slotSet.head -> topic
    }.groupBy(_._1).mapValuesStrict(_.map(_._2))

  /** The min number of persons for each topic that has a min number of persons */
  lazy val minNumberPerTopic: Map[Topic, Int] = constraints.collect {
    case TopicNeedsNumberOfPersons(t, min, _) => (t, min)
  }.toMap.withDefaultValue(0)

  /** The max number of persons for each topic that has a max number of persons */
  lazy val maxNumberPerTopic: Map[Topic, Int] = constraints.collect {
    case TopicNeedsNumberOfPersons(t, _, max) => (t, max)
  }.toMap.withDefaultValue(personsCount)

  /** For everyone, its preferences */
  lazy val preferencesPerPerson: Map[Person, Set[Preference]] = preferences.groupBy(_.person)

  lazy val toFormattedString: String = {
    val builder = new StringBuilder("Problem:\n")
    builder.append("  Slots:\n")
    slots.toSeq.sortBy(_.name).foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Topics:\n")
    topics.toSeq.sortBy(_.name).foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Persons:\n")
    persons.toSeq.sortBy(_.name).foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Constraints:\n")
    constraints.toSeq.sortBy(_.toString).foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Preferences:\n")
    preferences.toSeq.sortBy(_.toString).foreach(builder.append("    ").append(_).append("\n"))
    builder.toString
  }
}

