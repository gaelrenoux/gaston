package fr.renoux.gaston.model

/**
 * The more preferences are satisfied the better, but it's not mandatory. Preferences should implement `equals()` and
 * `hashCode()` to allow deduplication.
 */
trait Preference {

  /** Score you get each time you satisfy this constraint. Anti-preferences (stuff you would like no to happen) should
   * have a negative reward */
  def reward: Score

  def score(schedule: Schedule): Score

  def toLongString: String

  val isPersonal: Boolean

  def toAbstract: Product

}

object Preference {


  trait Anti extends Preference {
    assert(reward.value <= 0, s"AntiPreference should have a negative reward")
  }

  /** Trait for preferences which can be evaluated only on the global level, not slot by slot or record by record */
  trait GlobalLevel extends Preference {

    /** Indicates wether persons assignement matter for this preference. It's true in most case, but there are some
     * global-level preferences that only looks at how the topics are arranged. */
    def personsMatter: Boolean = true

    /** Score the schedule according to this preference. */
    def scoreSchedule(schedule: Schedule): Score

    override def score(schedule: Schedule): Score = scoreSchedule(schedule)
  }

  /** Trait for preferences which can be evaluated slot by slot, but not record by record */
  trait SlotLevel extends Preference {
    /** Score the slot schedule according to this preference */
    def scoreSlot(schedule: SlotSchedule): Score

    override def score(schedule: Schedule): Score = Score.sum(schedule.slotSchedulesList)(scoreSlot)
  }

  /** Trait for preferences which can be evaluated record by record */
  trait RecordLevel extends Preference {
    /** Score the record according to this preference */
    def scoreRecord(record: Record): Score

    override def score(schedule: Schedule): Score =
      Score.sum(schedule.slotSchedulesList.flatMap(_.recordsList))(scoreRecord)
  }

  /** Personal preferences are always at record level, and apply to a specific person. */
  trait Personal extends RecordLevel {
    /** A personal preference always references someone */
    val person: Person

    override val isPersonal: Boolean = true
  }

  /** Impersonal preferences are not linked to a single, specific person. Persons assignment may still matter though! */
  trait Impersonal extends Preference {
    override val isPersonal: Boolean = false
  }

  /** This is only used for pseudo-constraints: constraints represented as preferences. */
  val NecessaryPreferenceScore: Score = Score(-1000000000.0) // Cannot be negative infinity: we want breaking one constraint to be better than breaking two.

}
