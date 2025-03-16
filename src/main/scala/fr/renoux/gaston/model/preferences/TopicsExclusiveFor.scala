package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.*
import fr.renoux.gaston.util.ArraySet

/** The persons specifically mentioned cannot be on more than one of the topics inside that list (regardless of slot).
 *
 * This is often used as a soft-constraint (very high negative score, in order to make sure it's always respected).
 */
final case class TopicsExclusiveFor(topics: ArraySet[Topic], inclusions: ArraySet[Person], reward: Score = Preference.NecessaryPreferenceScore)
  extends Preference.GlobalLevel with Preference.Anti with Preference.Impersonal {
  // TODO add score scaling: the more times we break the rule, the lower it goes (e.g. it's -50 on two topics but -200 on three)

  assert(topics.size > 1, s"$this should contain more than one topic")

  override def scoreSchedule(schedule: Schedule): Score = {
    val groups: Iterable[Set[Person]] = schedule.personsByTopic.view.filterKeys(topics.contains).values.map(_.filter(inclusions.contains))
    groups.foldLeft((Set.empty[Person], Score.Zero)) { case ((found, score), ps) =>
      if (ps.exists(found)) (found, score + reward)
      else (found ++ ps, score)
    }._2
  }

  override def equals(o: Any): Boolean = o match {
    case that: TopicsExclusiveFor => this.topics.actualEquals(that.topics) && this.inclusions.actualEquals(that.inclusions) && this.reward == that.reward
    case _ => false
  }

  override def hashCode(): Int = (this.topics.actualHashCode, this.inclusions.actualHashCode, reward).hashCode()

  override lazy val toLongString: String = s"TopicsExclusiveFor(${topics.toGoodString}, $inclusions, $reward)"

  override lazy val toAbstract: (String, Seq[Int], Seq[Int], Double) =
    ("TopicsExclusiveFor", topics.toIdSet.toSeq.sorted, inclusions.toIdSet.toSeq.sorted, reward.value)
}

