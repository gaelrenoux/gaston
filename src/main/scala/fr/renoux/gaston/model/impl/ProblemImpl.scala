package fr.renoux.gaston.model.impl

import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.{Preference, _}
import fr.renoux.gaston.util.ArraySet.syntax._
import fr.renoux.gaston.util.BitMap.syntax._
import fr.renoux.gaston.util.CanGroupToMap.ops._
import fr.renoux.gaston.util.CollectionImplicits._
import fr.renoux.gaston.util.{ArraySet, BitMap}

/** A problem to solve. A schedule solves a problem. */
final class ProblemImpl(
    val slotSequences: Array[Array[Slot]],
    val topics: Array[Topic],
    val unassignedTopics: BitMap[Slot, Topic],
    val persons: Array[Person],
    val constraints: Array[Constraint],
    val preferences: Array[Preference]
) extends Problem {

  lazy val mandatoryTopicsByPerson: BitMap[Person, ArraySet[Topic]] =
    topics.toSeq.flatMap(t => t.mandatory.map(_ -> t)).groupToMap.mapValuesStrict(_.toArraySet).toBitMap(ArraySet.empty[Topic])

  lazy val forbiddenTopicsByPerson: BitMap[Person, ArraySet[Topic]] =
    topics.toSeq.flatMap(t => t.forbidden.map(_ -> t)).groupToMap.mapValuesStrict(_.toArraySet).toBitMap(ArraySet.empty[Topic])

  lazy val incompatibleTopicsByTopic: BitMap[Topic, ArraySet[Topic]] = {
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

    val m: Map[Topic, Seq[Topic]] = (conflictingMandatories ++ notSimultaneous).toSeq.groupToMap
    m.mapValuesStrict(_.toArraySet).toBitMap(ArraySet.empty[Topic])
  }

  lazy val incompatibleTopicsBySlot: BitMap[Slot, ArraySet[Topic]] = {
    val couples = for {
      slot <- slots
      topic <- topics
      if topic.mandatory.exists(!slot.personsPresent.contains(_))
      if topic.slots.exists(!_.contains(slot))
    } yield (slot, topic)
    val s: Set[(Slot, Topic)] = couples.toSet
    s.groupToMap.mapValuesStrict(_.toArraySet).toBitMap(ArraySet.empty[Topic])
  }

  lazy val simultaneousTopicByTopic: BitMap[Topic, ArraySet[Topic]] = {
    constraints.collect {
      case TopicsSimultaneous(ts) => ts.mapToArray(t => t -> (ts - t))
    }.flatten.toMap.toBitMap(ArraySet.empty[Topic])
  }

  lazy val linkedTopicsByTopic: BitMap[Topic, ArraySet[Topic]] = {
    constraints.collect {
      case TopicsSimultaneous(ts) => ts.mapToArray(t => t -> ts)
    }.flatten.toMap.toBitMap(ArraySet.empty[Topic])
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
