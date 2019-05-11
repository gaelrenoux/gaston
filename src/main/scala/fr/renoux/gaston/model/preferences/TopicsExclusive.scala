package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model._

/** No person (outside of the persons explicitly exempted from this rule) can be on more than one of the topics inside that list. */
case class TopicsExclusive(topics: Set[Topic], exemptions: Set[Person] = Set(), reward: Score = Preference.NecessaryPreferenceScore)
  extends Preference.Anti {

  /** Specific implementation, faster than the default */
  override def score(schedule: Schedule): Score = {
    val groups = schedule.personsPerTopic.filterKeys(topics.contains).values.view.map(_.filterNot(exemptions))
    groups.foldLeft((Set.empty[Person], Score.Zero)) { case ((found, score), ps) =>
      if (ps.exists(found)) (found, score + reward)
      else (found ++ ps, score)
    }._2
  }

}

