package fr.renoux.gaston.model.impl

import fr.renoux.gaston.model.{Preference, _}
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.util.CanGroupToMap._
import fr.renoux.gaston.util.CollectionImplicits._

/** A problem to solve. A schedule solves a problem. */
class ProblemImpl(
    val slotSequences: Seq[Seq[Slot]],
    val topics: Set[Topic],
    val persons: Set[Person],
    val constraints: Set[Constraint],
    val preferences: Set[Preference]
) extends Problem {

  val slots: Set[Slot] = slotSequences.flatten.toSet

  lazy val personsCount: Int = persons.size

  /** Max number of topics on a single slot */
  override val maxTopicCountPerSlot: Map[Slot, Int] = constraints.collect {
    case SlotMaxTopicCount(slot, count) => slot -> count
  }.toMap.withDefaultValue(Int.MaxValue)

  lazy val mandatoryTopicsPerPerson: Map[Person, Set[Topic]] =
    topics.flatMap(t => t.mandatory.map(_ -> t)).groupToMap.withDefaultValue(Set.empty)

  lazy val forbiddenTopicsPerPerson: Map[Person, Set[Topic]] =
    topics.flatMap(t => t.forbidden.map(_ -> t)).groupToMap.withDefaultValue(Set.empty)

  /** Indicates wether a person is available on a slot or not. */
  lazy val personSlotsPossibilities: Set[(Person, Slot)] = {
    val absences = constraints.collect { case PersonAbsence(p, s) => (p, s) }
    for {p <- persons; s <- slots if !absences((p, s))} yield (p, s)
  }

  lazy val slotsPerPerson: Map[Person, Set[Slot]] = personSlotsPossibilities.groupToMap

  lazy val personsPerSlot: Map[Slot, Set[Person]] = personSlotsPossibilities.map(_.swap).groupToMap

  lazy val personsCountPerSlot: Map[Slot, Int] = personsPerSlot.mapValuesStrict(_.size)

  lazy val incompatibleTopicsPerTopic: Map[Topic, Set[Topic]] = {
    val couples = for {
      topic1 <- topics
      topic2 <- topics
      if topic1.mandatory.intersect(topic2.mandatory).nonEmpty
    } yield (topic1, topic2)

    couples.groupToMap.withDefaultValue(Set.empty)
  }

  lazy val incompatibleTopicsPerSlot: Map[Slot, Set[Topic]] = {
    val couples = for {
      slot <- slots
      topic <- topics
      if topic.mandatory.exists(!personsPerSlot(slot).contains(_))
      // TODO add topic explicitely forbidden on the slot
    } yield (slot, topic)
    couples.groupToMap.withDefaultValue(Set.empty)
  }

  lazy val simultaneousTopicPerTopic: Map[Topic, Set[Topic]] = {
    constraints.collect {
      case TopicsSimultaneous(ts) => ts.map(t => t -> (ts - t))
    }.flatten.toMap.withDefaultValue(Set())
  }

  /** For each slots, the topics that must happen in that slot. Handles only topics with just one possible slot. */
  lazy val forcedTopicsPerSlot: Map[Slot, Set[Topic]] =
    constraints.collect {
      case TopicForcedSlot(topic, slotSet) if slotSet.size == 1 => slotSet.head -> topic
    }.groupBy(_._1).mapValuesStrict(_.map(_._2))

  lazy val preferencesPerPerson: Map[Person, Set[Preference.Personal]] = preferences.collect {
    case p: Preference.Personal => p
  }.groupBy(_.person).withDefaultValue(Set())

  lazy val toFormattedString: String = {
    val builder = new StringBuilder("Problem:\n")
    builder.append("  Slots:\n")
    slotsSeq.sortBy(_.name).foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Topics:\n")
    topicsSeq.sortBy(_.name).foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Persons:\n")
    personsSeq.sortBy(_.name).foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Constraints:\n")
    constraintsSeq.sortBy(_.toString).foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Preferences:\n")
    preferencesSeq.sortBy(_.toString).foreach(builder.append("    ").append(_).append("\n"))
    builder.toString
  }
}
