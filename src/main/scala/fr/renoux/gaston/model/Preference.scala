package fr.renoux.gaston.model

import scalaz.Scalaz._

/**
  * The more preferences are satisfied the better, but it's not mandatory. Preferences should implement [[equals()]] and
  * [[hashCode()]] to allow deduplication.
  */
trait Preference {

  /** Score you get each time you satisfy this constraint. Anti-preferences (stuff you would like no to happen) should
    * have a negative reward */
  def reward: Score

  def score(schedule: Schedule): Score
}

object Preference {


  trait Anti extends Preference {
    assert(reward.value <= 0, s"AntiPreference $this should have a negative reward")
  }

  /** Trait for preferences which can be evaluated only on the global level, not slot by slot or record by record */
  trait GlobalLevel extends Preference {
    /** Score the schedule according to this preference. */
    def scoreSchedule(schedule: Schedule): Score

    override def score(schedule: Schedule): Score = scoreSchedule(schedule)
  }

  /** Trait for preferences which can be evaluated slot by slot, but not record by record */
  trait SlotLevel extends Preference {
    /** Score the slot schedule according to this preference */
    def scoreSlot(schedule: SlotSchedule): Score

    override def score(schedule: Schedule): Score = schedule.slotSchedulesList.map(scoreSlot).suml
  }

  /** Trait for preferences which can be evaluated record by record */
  trait RecordLevel extends Preference {
    /** Score the record according to this preference */
    def scoreRecord(record: Record): Score

    override def score(schedule: Schedule): Score = schedule.slotSchedulesList.flatMap(_.recordsList).map(scoreRecord).suml
  }

  /** Personal preferences are always at record level */
  trait Personal extends RecordLevel {
    /** A personal preference always references someone */
    val person: Person
  }

  val NecessaryPreferenceScore: Score = Score.NegativeInfinity

}
