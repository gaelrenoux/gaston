package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model._
import fr.renoux.gaston.util.BitSet

/** No person (outside of the persons explicitly exempted from this rule) can be on more than one of the topics inside that list (regardless of slot).
  *
  * This is often used as a soft-constraint (very high negative score, in order to make sure it's always respected).
  */
final case class TopicsExclusive(topics: BitSet[Topic], exemptions: BitSet[Person], reward: FlatScore = Preference.NecessaryPreferenceScore)
  extends Preference.GlobalLevel with Preference.Anti with Preference.Impersonal {
  // TODO add score scaling: the more times we break the rule, the lower it goes (e.g. it's -50 on two topics but -200 on three)

  assert(topics.size > 1, s"$this should contain more than one topic")

  override def scoreSchedule(schedule: Schedule): FlatScore = {
    val groups = schedule.personsByTopic.view.filterKeys(topics.contains).values.map(_.filterNot(exemptions.contains))
    groups.foldLeft((Set.empty[Person], FlatScore.Zero)) { case ((found, score), ps) =>
      if (ps.exists(found)) (found, score + reward)
      else (found ++ ps, score)
    }._2
  }

  override def equals(o: Any): Boolean = o match {
    case that: TopicsExclusive => this.topics.actualEquals(that.topics) && this.exemptions.actualEquals(that.exemptions) && this.reward == that.reward
    case _ => false
  }

  override def hashCode(): Int = (this.topics.actualHashCode, this.exemptions.actualHashCode, reward).hashCode()

  override lazy val toLongString: String = s"TopicsExclusive($topics${if (exemptions.nonEmpty) s"$exemptions, " else ""}, $reward)"

  override lazy val toAbstract: (String, Seq[Int], Seq[Int], Double) =
    ("TopicsExclusive", topics.toIdSet.toSeq.sorted, exemptions.toIdSet.toSeq.sorted, reward.value)
}

