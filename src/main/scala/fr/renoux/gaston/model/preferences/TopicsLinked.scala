package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.{Preference, _}
import fr.renoux.gaston.util.BitSet

/** Every person on any of these topics, must be on all of these topics. */
final case class TopicsLinked(topics: BitSet[Topic], reward: Score = Preference.NecessaryPreferenceScore)
  extends Preference.GlobalLevel with Preference.Anti {
  private val topicsCount: Int = topics.size

  assert(topicsCount > 1, s"$this should contain more than one topic")

  override def scoreSchedule(schedule: Schedule): Score = {
    val groups: Iterable[Set[Person]] = schedule.personsByTopic.view.filterKeys(topics.contains).values
    if (groups.isEmpty) {
      // all topics unscheduled, that's fine
      Score.Zero
    } else if (groups.size < topicsCount) {
      // only some topics are scheduled, that's not good
      throw new IllegalStateException(s"WROOOOOOOONG: ${schedule.toFormattedString}")
    } else {
      val persons = groups.head
      if (groups.tail.exists(_ != persons)) {
        throw new IllegalStateException(s"WROOOOOOOONG: ${schedule.toFormattedString}")
      } else Score.Zero
    }
  }

  override def equals(o: Any): Boolean = o match {
    case that: TopicsLinked => this.topics.actualEquals(that.topics) && this.reward == that.reward
    case _ => false
  }

  override def hashCode(): Int = (this.topics.actualHashCode, reward).hashCode()
}

