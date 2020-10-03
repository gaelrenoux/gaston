package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model._

/** No person (outside of the persons explicitly exempted from this rule) can be on more than one of the topics inside that list (regardless of slot). */
case class TopicsExclusive(topics: Set[Topic], exemptions: Set[Person] = Set.empty, reward: Score = Preference.NecessaryPreferenceScore)
  extends Preference.GlobalLevel with Preference.Anti {

  override def scoreSchedule(schedule: Schedule): Score = {
    val groups = schedule.personsByTopic.view.filterKeys(topics.contains).values.map(_.filterNot(exemptions))
    groups.foldLeft((Set.empty[Person], Score.Zero)) { case ((found, score), ps) =>
      if (ps.exists(found)) (found, score + reward)
      else (found ++ ps, score)
    }._2
  }
}

