package fr.renoux.gaston.model.impl

import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.{Preference, _}
import fr.renoux.gaston.util.CanGroupToMap.ops._
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

  lazy val mandatoryTopicsPerPerson: Map[Person, Set[Topic]] =
    topics.flatMap(t => t.mandatory.map(_ -> t)).groupToMap.withDefaultValue(Set.empty)

  lazy val forbiddenTopicsPerPerson: Map[Person, Set[Topic]] =
    topics.flatMap(t => t.forbidden.map(_ -> t)).groupToMap.withDefaultValue(Set.empty)

  lazy val incompatibleTopicsPerTopic: Map[Topic, Set[Topic]] = {
    val conflictingMandatories = for {
      topic1 <- topics
      topic2 <- topics
      if topic1.mandatory.intersect(topic2.mandatory).nonEmpty
    } yield (topic1, topic2)

    val notSimultaneous = constraints.collect {
      case TopicsNotSimultaneous(ts) => ts.cross(ts)
    }.flatten

    (conflictingMandatories ++ notSimultaneous).groupToMap.withDefaultValue(Set.empty)
  }

  lazy val incompatibleTopicsPerSlot: Map[Slot, Set[Topic]] = {
    val couples = for {
      slot <- slots
      topic <- topics
      if topic.mandatory.exists(!slot.personsPresent.contains(_))
      if topic.slots.exists(!_.contains(slot))
    } yield (slot, topic)
    couples.groupToMap.withDefaultValue(Set.empty)
  }

  lazy val simultaneousTopicPerTopic: Map[Topic, Set[Topic]] = {
    constraints.collect {
      case TopicsSimultaneous(ts) => ts.map(t => t -> (ts - t))
    }.flatten.toMap.withDefaultValue(Set.empty)
  }

  lazy val preferencesPerPerson: Map[Person, Set[Preference.Personal]] = preferences.collect {
    case p: Preference.Personal => p
  }.groupBy(_.person).withDefaultValue(Set.empty)

  lazy val toFormattedString: String = {
    val builder = new StringBuilder("Problem:\n")
    builder.append("  Slots:\n")
    slotsList.sortBy(_.name).foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Topics:\n")
    topicsList.sortBy(_.name).foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Persons:\n")
    personsList.sortBy(_.name).foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Constraints:\n")
    constraintsList.sortBy(_.toString).foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Preferences:\n")
    preferencesList.sortBy(_.toString).foreach(builder.append("    ").append(_).append("\n"))
    builder.toString
  }
}
