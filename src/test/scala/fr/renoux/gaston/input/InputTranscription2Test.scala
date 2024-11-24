package fr.renoux.gaston.input

import fr.renoux.gaston.TestBase
import fr.renoux.gaston.TestUtils.*


class InputTranscription2Test extends TestBase {

  "Correct model" - {
    val input: InputModel = InputLoader.fromClassPath("transcription-test.conf").force
    val transcription = InputTranscription2(input)
    "errors" in {
      transcription.errors.isEmpty should be(true)
    }

    "slots" in {
      transcription.slotsCount should be(6)

      println("\nSlots ------------------------------------")
      println("Count: " + transcription.slotsCount.toPrettyString)
      println("Names: " + transcription.slotsNames.toPrettyString)
      println("Max topics: " + transcription.slotsMaxTopics.toPrettyString)
      println("Followups: " + transcription.slotToNextSlot.toPrettyString)
      println()
    }

    "persons" in {
      transcription.personsCount should be(3)

      println("\nPersons ----------------------------------")
      println("Count: " + transcription.personsCount.toPrettyString)
      println("Names: " + transcription.personsNames.toPrettyString)
      println("Weights: " + transcription.personsWeights.toPrettyString)
      println("Base scores: " + transcription.personsBaseScores.toPrettyString)
      println()
    }

    "topics" in {
      transcription.topicsCount should be(6 + 14) // unassigned + normal (with occ/long)
      println("\nTopics -----------------------------------")
      println("Count: " + transcription.topicsCount.toPrettyString)
      println("Names: " + transcription.topics.topicsNames.toPrettyString)
      println("Forced: " + transcription.topics.topicsForced.toPrettyString)
      println("Max: " + transcription.topics.topicsMax.toPrettyString)
      println("Min: " + transcription.topics.topicsMin.toPrettyString)
      println("Mandatories: " + transcription.topics.topicsMandatories.toPrettyString)
      println("Allowed slots: " + transcription.topics.topicsAllowedSlots.toPrettyString)
      println("Followups: " + transcription.topics.topicsFollowup.toPrettyString)
      println()
    }
  }

  "Real-world" - {
    "udocon2017" ignore {
      val input: InputModel = InputLoader.fromClassPath("udocon2017/uc17-from-table.conf").force
      val transcription = InputTranscription2(input)
      transcription.errors.isEmpty should be(true)
      transcription.slotsCount should be(5)
      transcription.personsCount should be(27)
      transcription.topicsCount should be(5 + 26) // unassigned + normal

      println("\nSlots ------------------------------------")
      println("Count: " + transcription.slotsCount.toPrettyString)
      println("Names: " + transcription.slotsNames.toPrettyString)
      println("Max topics: " + transcription.slotsMaxTopics.toPrettyString)
      println("Followups: " + transcription.slotToNextSlot.toPrettyString)

      println("\nPersons ----------------------------------")
      println("Count: " + transcription.personsCount.toPrettyString)
      println("Names: " + transcription.personsNames.toPrettyString)
      println("Weights: " + transcription.personsWeights.toPrettyString)
      println("Base scores: " + transcription.personsBaseScores.toPrettyString)

      println("\nTopics -----------------------------------")
      println("Count: " + transcription.topicsCount.toPrettyString)
      println("Names: " + transcription.topics.topicsNames.toPrettyString)
      println("Max: " + transcription.topics.topicsMax.toPrettyString)
      println("Min: " + transcription.topics.topicsMin.toPrettyString)
      println("Mandatories: " + transcription.topics.topicsMandatories.toPrettyString)
    }
  }
}
