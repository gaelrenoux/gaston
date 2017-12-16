package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.{Schedule, Score}

/**
  * The more preferences are satisfied the better, but it's not mandatory. Preferences should implement #equals and
  * #hashCode to allow deduplication.
  */
trait Preference {
  /** Score you get each time you satisfy this constraint. Anti-preferences (stuff you would like no to happen) should
    * have a negative reward */
  def reward: Score

  def score(schedule: Schedule): Score
}
