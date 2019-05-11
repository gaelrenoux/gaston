package fr.renoux.gaston.model

/**
  * The more preferences are satisfied the better, but it's not mandatory. Preferences should implement [[equals()]] and
  * [[hashCode()]] to allow deduplication.
  */
trait Preference {

  /** Score you get each time you satisfy this constraint. Anti-preferences (stuff you would like no to happen) should
    * have a negative reward */
  def reward: Score

  /** Score the schedule according to this preference */
  def score(schedule: Schedule): Score
}

object Preference {

  trait Personal extends Preference {
    /** A personal preference always references someone */
    val person: Person
  }

  trait Anti extends Preference {
    assert(reward.value <= 0, s"AntiPreference $this should have a negative reward")
  }

  /** Trait for preferences which can be evaluated slot by slot */
  trait SlotLevel extends Preference {

    /** Score the schedule according to this preference. Default implementation can be overriden. */
    def score(schedule: Schedule): Score = schedule.slotSchedules.map(scoreSlot).sum

    /** Score the slot schedule according to this preference */
    def scoreSlot(schedule: SlotSchedule): Score
  }

  val NecessaryPreferenceScore: Score = Score.PersonTotalScore.negative * 100

}