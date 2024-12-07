package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase

class ScheduleTest extends TestBase {

  val height = 3
  val width = 4
  given countSlots: CountAll[SlotId] = CountAll[SlotId](2)
  given countTopics: CountAll[TopicId] = CountAll[TopicId](4)
  given countPersons: CountAll[PersonId] = CountAll[PersonId](4)
  import ScheduleMaker.mkSchedule

  val schedule = mkSchedule {
    0 slot {
      0 topic (0, 1, 2)
      1 topic (3)
    }
    1 slot {
      3 topic (0, 1, 2, 3)
    }
  }

  "planning" in {
    val plan = schedule.planning.toMap.view.mapValues(_.toSet).toMap
    plan should be(Map(0 -> Set(0, 1), 1 -> Set(3)))
  }

  "assignment" in {
    val ass = schedule.personsToTopics.toMap.view.mapValues(_.toSet).toMap
    ass should be(Map(0 -> Set(0, 3), 1 -> Set(0, 3), 2 -> Set(0, 3), 3 -> Set(1, 3)))
  }

  "topicsToPersons" in {
    val ttp = schedule.topicsToPersons.toMap.view.mapValues(_.toSet).toMap
    ttp should be(Map(0 -> Set(0, 1, 2), 1 -> Set(3), 2 -> Set(), 3 -> Set(0, 1, 2, 3)))
  }

  "topicsPresent" in {
    val tp = schedule.topicsPresent.toSet
    tp should be(Set(0, 1, 3))
  }

}
