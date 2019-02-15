package fr.renoux.gaston.model

/* TODO scores could be cached. In the Schedule, have a set of ScheduleSot, scorable separately for most. And transformations usually alter only one slot at a time. */

/**
  * The more preferences are satisfied the better, but it's not mandatory. Preferences should implement [[equals()]] and
  * [[hashCode()]] to allow deduplication.
  */
trait Preference {

  /** A preference always references someone */
  val person: Person

  /** Score you get each time you satisfy this constraint. Anti-preferences (stuff you would like no to happen) should
    * have a negative reward */
  def reward: Score

  /** Score the schedule according to this preference */
  def score(schedule: Schedule): Score
}

trait AntiPreference extends Preference {
  assert(reward.value <= 0, s"AntiPreference $this should have a negative reward")
}
