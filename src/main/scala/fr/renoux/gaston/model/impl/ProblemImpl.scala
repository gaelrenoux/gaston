package fr.renoux.gaston.model.impl

import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.{Preference, _}
import fr.renoux.gaston.util.BitMap
import fr.renoux.gaston.util.CanGroupToMap.ops._
import fr.renoux.gaston.util.CollectionImplicits._

/** A problem to solve. A schedule solves a problem. */
final class ProblemImpl(
    val slotSequences: Array[Array[Slot]],
    val topics: Array[Topic],
    val unassignedTopics: BitMap[Slot, Topic],
    val persons: Array[Person],
    val constraints: Array[Constraint],
    val preferences: Array[Preference]
)(implicit val counts: Counts) extends Problem {

  val topicsSet: Set[Topic] = topics.toSet

  lazy val personsCount: Int = persons.length

  lazy val mandatoryTopicsByPerson: BitMap[Person, Set[Topic]] =
    topicsSet.flatMap(t => t.mandatory.map(_ -> t)).groupToMap.toBitMap(Set.empty)

  lazy val forbiddenTopicsByPerson: BitMap[Person, Set[Topic]] =
    topicsSet.flatMap(t => t.forbidden.map(_ -> t)).groupToMap.toBitMap(Set.empty)

  lazy val incompatibleTopicsByTopic: BitMap[Topic, Set[Topic]] = {
    val conflictingMandatories = for {
      topic1 <- topics
      topic2 <- topics
      if topic1.mandatory.intersect(topic2.mandatory).nonEmpty
    } yield (topic1, topic2)

    val notSimultaneous = constraints.collect {
      case TopicsNotSimultaneous(ts) =>
        val topics = this.topics.filter(ts)
        topics.cross(topics)
    }.flatten

    val s = (conflictingMandatories ++ notSimultaneous).toSet // compiler needs the intermediate value for some reason
    s.groupToMap.toBitMap(Set.empty)
  }

  lazy val incompatibleTopicsBySlot: BitMap[Slot, Set[Topic]] = {
    val couples = for {
      slot <- slots
      topic <- topics
      if topic.mandatory.exists(!slot.personsPresent.contains(_))
      if topic.slots.exists(!_.contains(slot))
    } yield (slot, topic)
    val s = couples.toSet // compiler needs the intermediate value for some reason
    s.groupToMap.toBitMap(Set.empty)
  }

  lazy val simultaneousTopicByTopic: BitMap[Topic, Set[Topic]] = {
    constraints.collect {
      case TopicsSimultaneous(ts) => ts.map(t => t -> (ts - t))
    }.flatten.toMap.toBitMap(Set.empty)
  }

  lazy val toFormattedString: String = {
    val builder = new StringBuilder("Problem:\n")
    builder.append("  Slots:\n")
    slots.foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Topics:\n")
    topics.sortBy(_.name).foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Persons:\n")
    persons.sortBy(_.name).foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Constraints:\n")
    constraints.sortBy(_.toString).foreach(builder.append("    ").append(_).append("\n"))
    builder.append("  Preferences:\n")
    preferences.sortBy(_.toString).foreach(builder.append("    ").append(_).append("\n"))
    builder.toString
  }
}
