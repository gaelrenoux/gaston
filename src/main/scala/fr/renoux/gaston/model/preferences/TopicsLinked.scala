package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.{Preference, *}
import fr.renoux.gaston.util.BitSet

/** Every person on any of these topics, must be on all of these topics. Note that the 'reward' is applied for each anomaly. */
final case class TopicsLinked(topics: BitSet[Topic], reward: Score = Preference.NecessaryPreferenceScore)
  extends Preference.GlobalLevel with Preference.Anti with Preference.Impersonal {
  // TODO Add unit test

  assert(topics.size > 1, s"$this should contain more than one topic")

  override def scoreSchedule(schedule: Schedule): Score = {
    val groups: Iterable[Set[Person]] = schedule.personsByTopic.view.filterKeys(topics.contains).values
    if (groups.isEmpty) Score.Zero else {
      val persons = groups.foldLeft(Set.empty[Person])(_ ++ _)
      groups.foldLeft(Score.Zero) { (score, current) =>
        val anomalyCount = (persons diff current).size + (current diff persons).size
        score + (reward * anomalyCount)
      }
    }
  }

  override def equals(o: Any): Boolean = o match {
    case that: TopicsLinked => this.topics.actualEquals(that.topics) && this.reward == that.reward
    case _ => false
  }

  override def hashCode(): Int = (this.topics.actualHashCode, reward).hashCode()

  override lazy val toLongString: String = s"TopicsLinked($topics, $reward)"

  override lazy val toAbstract: (String, Seq[Int], Double) = ("TopicsLinked", topics.toIdSet.toSeq.sorted, reward.value)
}

