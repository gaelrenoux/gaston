package fr.renoux.gaston.model.impl

import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.{Preference, _}
import fr.renoux.gaston.util.BitMap
import fr.renoux.gaston.util.CanGroupToMap.ops._
import fr.renoux.gaston.util.CollectionImplicits._

/** A problem to solve. A schedule solves a problem. */
final class ProblemImpl(
    val slotSequences: Seq[Seq[Slot]],
    val topics: Set[Topic],
    val unassignedTopics: BitMap[Slot, Topic],
    val persons: Set[Person],
    val constraints: Set[Constraint],
    val preferences: Set[Preference]
)(implicit val counts: Counts) extends Problem {

  val slots: Set[Slot] = slotSequences.flatten.toSet

  lazy val personsCount: Int = persons.size

  lazy val mandatoryTopicsByPerson: BitMap[Person, Set[Topic]] =
    topics.flatMap(t => t.mandatory.map(_ -> t)).groupToMap.toBitMap(Set.empty)

  lazy val forbiddenTopicsByPerson: BitMap[Person, Set[Topic]] =
    topics.flatMap(t => t.forbidden.map(_ -> t)).groupToMap.toBitMap(Set.empty)

  lazy val incompatibleTopicsByTopic: BitMap[Topic, Set[Topic]] = {
    val conflictingMandatories = for {
      topic1 <- topics
      topic2 <- topics
      if topic1.mandatory.intersect(topic2.mandatory).nonEmpty
    } yield (topic1, topic2)

    val notSimultaneous = constraints.collect {
      case TopicsNotSimultaneous(ts) =>
        val topics = this.topics.filter(ts.contains)
        topics.cross(topics)
    }.flatten

    (conflictingMandatories ++ notSimultaneous).groupToMap.toBitMap(Set.empty)
  }

  lazy val incompatibleTopicsBySlot: BitMap[Slot, Set[Topic]] = {
    val couples = for {
      slot <- slots
      topic <- topics
      if !topic.virtual // Virtual topics are never moved anyway
      if topic.mandatory.exists(!slot.personsPresent.contains(_)) || topic.slots.exists(!_.contains(slot))
    } yield (slot, topic)
    couples.groupToMap.toBitMap(Set.empty)
  }

  lazy val simultaneousTopicByTopic: BitMap[Topic, Set[Topic]] = {
    constraints.collect {
      case TopicsSimultaneous(ts) => ts.map(t => t -> (ts - t))
    }.flatten.toMap.toBitMap(Set.empty)
  }

  lazy val preferencesByPerson: BitMap[Person, Set[Preference.Personal]] = preferences.collect {
    case p: Preference.Personal => p
  }.groupBy(_.person).toBitMap(Set.empty)

  lazy val toFormattedString: String = {
    val builder = new StringBuilder("Problem:\n")
    builder.append("  Slots:\n")
    slotsList.sortBy(_.name).map(_.toLongString).foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Topics:\n")
    topicsList.sortBy(_.name).map(_.toLongString).foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Persons:\n")
    personsList.sortBy(_.name).map(_.toLongString).foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Constraints:\n")
    constraintsList.map(_.toLongString).sorted.foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Preferences:\n")
    preferencesList.map(_.toLongString).sorted.foreach(builder.append("    ").append(_).append("\n"))
    builder.toString
  }
}
