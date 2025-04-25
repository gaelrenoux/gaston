package fr.renoux.gaston.model.constraints

import fr.renoux.gaston.model.*
import fr.renoux.gaston.util.ArraySet

/** Among the topics in the list, only a specific number of them can be present in the schedule. */
final case class TopicsLimitedCount(topics: ArraySet[Topic], count: Int) extends Constraint {

  assert(topics.size > count, s"$this should contain more topics than the target count of topic")

  override def isHardCoded: Boolean = false

  override val isApplicableToPartialSchedule: Boolean = true

  override def isRespected(schedule: Schedule): Boolean = {
    val scheduledCount = schedule.scheduledTopics.count(topics.contains)
    scheduledCount <= count
  }

  override lazy val toLongString: String = s"TopicsMaxCount(${topics.toGoodString}, $count)"

  override lazy val toAbstract: (String, Any, Int) = ("TopicsMaxCount", topics.toIdSet.toSeq.sorted, count)
}

