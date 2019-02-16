package fr.renoux.gaston.model

/** A view on the schedule, on one specific slot */
case class SlotSchedule(
    schedule: Schedule,
    slot: Slot
) {
  lazy val records: Set[Schedule.Record] = schedule.records.filter(_.slot == slot)
  lazy val topics: Set[Topic] = schedule.topicsPerSlot(slot)
}
