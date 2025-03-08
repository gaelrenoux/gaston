package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase
import fr.renoux.gaston.input.InputModel
import fr.renoux.gaston.input.InputLoader
import fr.renoux.gaston.input.InputTranscription2
import fr.renoux.gaston.TestUtils.*
import com.softwaremill.quicklens.*
import ScheduleMaker.mkSchedule

class SlotAssignmentTest extends TestBase {
  // TODO Fill in

  "Basics " - {
    given countSlots: CountAll[SlotId] = CountAll[SlotId](2)

    given countTopics: CountAll[TopicId] = CountAll[TopicId](4)

    given countPersons: CountAll[PersonId] = CountAll[PersonId](4)

    val baseProblem = SmallProblemMaker.make

    import ScheduleMaker.mkSchedule

    val schedule = mkSchedule(baseProblem) {
      0.slot {
        0.topic(0, 1, 2)
        1.topic(3)
      }
      1.slot {
        3.topic(0, 1, 2, 3)
      }
    }

    "equals" in {
      (schedule.slotsToAssignment(0) == schedule.slotsToAssignment(0)) should be(true)
      val expected = mkSchedule(baseProblem) {
        0.slot {
          0.topic(0, 1, 2)
          1.topic(3)
        }
        1.slot {
          3.topic(0, 1, 2, 3)
        }
      }
      (schedule.slotsToAssignment(0) == expected.slotsToAssignment(0)) should be(true)
      (expected.slotsToAssignment(0) == schedule.slotsToAssignment(0)) should be(true)

      val notExpected = mkSchedule(baseProblem) {
        0.slot {
          0.topic(0, 1, 3)
          1.topic (2)
        }
        1.slot {
          3.topic(0, 1, 2, 3)
        }
      }
      (schedule.slotsToAssignment(0) == notExpected.slotsToAssignment(0)) should be(false)
      (notExpected.slotsToAssignment(0) == schedule.slotsToAssignment(0)) should be(false)
    }
  }

}
