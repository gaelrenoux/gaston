package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase


class ScheduleTest extends TestBase {

  val height = 3
  val width = 4
  given countSlots: CountAll[SlotId] = CountAll[SlotId](2)
  given countTopics: CountAll[TopicId] = CountAll[TopicId](3)
  given countPersons: CountAll[PersonId] = CountAll[PersonId](4)

  val scheduleSeq: Seq[Seq[Seq[Boolean]]] = Seq(
    Seq(
      Seq(true, true, true, false),
      Seq(false, false, false, false),
      Seq(false, false, false, false)
    ),
    Seq(
      Seq(false, false, false, false),
      Seq(true, false, false, true),
      Seq(false, true, true, false)
    )
  )

  val scheduleMatrix = IdMatrix3.unsafeFrom[SlotId, TopicId, PersonId, Boolean](scheduleSeq)
  val schedule = Schedule(scheduleMatrix)

  "personToTopics" in {
    val ptt = schedule.personToTopics
    ptt.toMap.view.mapValues(_.toSet).toMap should be(Map(0 -> Set(0, 1), 1 -> Set(0, 2), 2 -> Set(0, 2), 3 -> Set(1)))
  }

  "topicsToPersons" in {
    val ttp = schedule.topicsToPersons
    ttp.toMap.view.mapValues(_.toSet).toMap should be(Map(0 -> Set(0, 1, 2), 1 -> Set(0, 3), 2 -> Set(1, 2)))
  }

  "topicsPresent" in {
    val tp = schedule.topicsPresent
    tp.toSet should be(Set(0, 1, 2))
  }

}
