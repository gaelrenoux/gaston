package fr.renoux.gaston.input

import fr.renoux.gaston.TestBase
import fr.renoux.gaston.TestUtils.*
import fr.renoux.gaston.model2.*


class InputTranscription2Test extends TestBase {

  val maxCount = Count.maxCount[Id].value
  val emptyIdSet = SmallIdSet.empty[Id]
  val fullIdSet = SmallIdSet.full[Id]

  "Correct model" - {
    val input: InputModel = InputLoader.fromClassPath("transcription-test.conf").force
    val transcription = InputTranscription2(input)
    val defMaxPersonsPerTopic = transcription.settings.defaultMaxPersonsPerTopic
    val defMinPersonsPerTopic = transcription.settings.defaultMinPersonsPerTopic
    val defMaxTopicsPerSlot = transcription.settings.defaultMaxTopicsPerSlot.get
    "errors" in {
      transcription.errors.isEmpty should be(true)
    }

    "settings" in {
      defMaxPersonsPerTopic should be(6)
      defMinPersonsPerTopic should be(4)
      defMaxTopicsPerSlot should be(50)
    }

    "slots" in {
      transcription.slotsCount should be(6)
      transcription.slotsNames.toSeq should be(
        Seq("D1-afternoon", "D1-evening", "D2-afternoon", "D3-afternoon", "D3-evening", "D3-night")
      )
      println(">>>>>" + transcription.slotsMaxTopics.toSeq)
      transcription.slotsMaxTopics.toSeq should be(
        Seq(defMaxTopicsPerSlot, 5, defMaxTopicsPerSlot, defMaxTopicsPerSlot, defMaxTopicsPerSlot, defMaxTopicsPerSlot)
      )
      transcription.slotToNextSlot.toSeq should be(Seq(1, SlotId.None, SlotId.None, 4, 5, SlotId.None))
    }

    "persons" in {
      transcription.personsCount should be(3)
      transcription.personsNames.toSeq should be(Seq("Albert", "Bianca", "Charly"))
      transcription.personsWeights.toSeq should be(Seq(Weight.Default, 1.0, 2.0))
      transcription.personsBaseScores.toSeq should be(Seq(Score.Zero, 20, Score.Zero))
    }

    "topics" in {
      transcription.topicsCount should be(6 + 14) // unassigned + normal (with occ/long)
      transcription.topics.topicsNames.toSeq should be(
        Seq("D1-afternoon", "D1-evening", "D2-afternoon", "D3-afternoon", "D3-evening", "D3-night").map(s =>
          s"@Unassigned ($s)"
        ) ++
          Seq("Alpha", "Beta", "Gamma", "Delta") ++
          Seq("Epsilon #1", "Epsilon #2", "Eta ~1", "Eta ~2") ++
          Seq("Theta #1 ~1", "Theta #1 ~2", "Theta #2 ~1", "Theta #2 ~2", "Theta #3 ~1", "Theta #3 ~2")
      )
      transcription.topics.topicsForced.toSet should be((0 to 5).toSet ++ Set(8) ++ (14 to 19).toSet)
      transcription.topics.topicsMax.toSeq should be(
        Seq.fill(6)(maxCount) ++
          Seq(defMaxPersonsPerTopic, 5, defMaxPersonsPerTopic, defMaxPersonsPerTopic) ++
          Seq(defMaxPersonsPerTopic, defMaxPersonsPerTopic, defMaxPersonsPerTopic, defMaxPersonsPerTopic) ++
          Seq.fill(6)(7)
      )
      transcription.topics.topicsMin.toSeq should be(
        Seq.fill(6)(0) ++
          Seq(defMinPersonsPerTopic, defMinPersonsPerTopic, 2, defMinPersonsPerTopic) ++
          Seq(defMinPersonsPerTopic, defMinPersonsPerTopic, defMinPersonsPerTopic, defMinPersonsPerTopic) ++
          Seq.fill(6)(5)
      )
      transcription.topics.topicsMandatories.toSeq should be(
        Seq.fill(6)(emptyIdSet) ++
          Seq(SmallIdSet(0, 1), emptyIdSet, emptyIdSet, SmallIdSet(0)) ++
          Seq(emptyIdSet, emptyIdSet, emptyIdSet, emptyIdSet) ++
          Seq.fill(6)(SmallIdSet(2))
      )
      transcription.topics.topicsAllowedSlots.toSeq should be(
        (0 to 5).map(SmallIdSet(_)) ++
          Seq(fullIdSet, fullIdSet, fullIdSet, SmallIdSet(1, 4)) ++
          Seq(fullIdSet, fullIdSet, fullIdSet, fullIdSet) ++
          Seq.fill(6)(fullIdSet)
      )
      transcription.topics.topicsFollowup.toSeq should be(
        Seq.fill(6)(TopicId.None) ++
          Seq(TopicId.None, TopicId.None, TopicId.None, TopicId.None) ++
          Seq(TopicId.None, TopicId.None, 13, TopicId.None) ++
          Seq(15, TopicId.None, 17, TopicId.None, 19, TopicId.None)
      )
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
