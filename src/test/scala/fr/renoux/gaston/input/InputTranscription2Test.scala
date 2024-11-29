package fr.renoux.gaston.input

import fr.renoux.gaston.TestBase
import fr.renoux.gaston.TestUtils.*
import fr.renoux.gaston.model2.*
import fr.renoux.gaston.util.{Count as _, *}


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
      transcription.errors should be(Set())
    }

    "settings" in {
      defMaxPersonsPerTopic should be(6)
      defMinPersonsPerTopic should be(4)
      defMaxTopicsPerSlot should be(50)

      transcription.settingsUnassignedPrefByPerson.toSeq.map(_.value.round) should be(Seq(-100, -1, -72))
    }

    "slots" in {
      transcription.slotsCount should be(6)
      transcription.slotsNames.toSeq should be(
        Seq("D1-afternoon", "D1-evening", "D2-afternoon", "D3-afternoon", "D3-evening", "D3-night")
      )
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

    "constraints" - {
      "topicsSimultaneous" in {
        transcription.constraints.topicsSimultaneous.size should be (6 + 14)
        transcription.constraints.topicsSimultaneous.toSeq should be(
          Seq.fill(6)(emptyIdSet) ++
            Seq(SmallIdSet(7), SmallIdSet(6), emptyIdSet, emptyIdSet) ++
            Seq(emptyIdSet, emptyIdSet, emptyIdSet, emptyIdSet) ++
            Seq.fill(6)(emptyIdSet)
        )
      }

      "topicsNotSimultaneous" in {
        transcription.constraints.topicsNotSimultaneous.toSeq should be(
          Seq.fill(6)(emptyIdSet) ++
            Seq(emptyIdSet, emptyIdSet, emptyIdSet, emptyIdSet) ++
            Seq(SmallIdSet(11, 12, 13), SmallIdSet(10, 12, 13), SmallIdSet(10, 11, 13), SmallIdSet(10, 11, 12)) ++
            Seq.fill(6)(emptyIdSet)
        )
      }
    }

    // TODO add some preferences for the test
    "preferences" - {
      import transcription.given

      "prefsPersonTopic" in {
        transcription.preferences.prefsPersonTopic.toSeq2.mapMap(_.value.round) should be(Seq(
          Seq.fill(6)(-100) ++ Seq.fill(14)(0),
          Seq.fill(6)(-1) ++ Seq(0, Score.MinReward, Score.MinReward, Score.MinReward) ++ Seq(Score.MinReward, Score.MinReward, 0, 0) ++ Seq.fill(6)(0),
          Seq.fill(6)(-72) ++ Seq.fill(4)(0) ++ Seq(0, 0, Score.MinReward, Score.MinReward) ++ Seq.fill(6)(0)
        ))
      }

      "prefsPersonPerson" in {
        transcription.preferences.prefsPersonPerson.toSeq2.mapMap(_.value.round) should be(Seq(
          Seq.fill(3)(Score.Zero),
          Seq.fill(3)(Score.Zero),
          Seq.fill(3)(Score.Zero)
        ))
      }

      "prefsTopicsExclusive" in {
        val prefsTopicsExclusive = transcription.preferences.prefsTopicsExclusive.toSeq.map(_.toSeq2)
        val expectedTop = Seq.tabulate(6) { id => Seq.tabulate(id + 1) { id2 => if (id2 == id) Score.Zero else -50 } } 
        val expectedLeft =  Seq.fill(6)(Score.Zero)
        prefsTopicsExclusive(0).take(6) should be(expectedTop)
        prefsTopicsExclusive(1).take(6) should be(expectedTop)
        prefsTopicsExclusive(2).take(6) should be(expectedTop)
        prefsTopicsExclusive(0).drop(6).map(_.take(6)) should be(Seq.fill(14)(expectedLeft))
        prefsTopicsExclusive(1).drop(6).map(_.take(6)) should be(Seq.fill(14)(expectedLeft))
        prefsTopicsExclusive(2).drop(6).map(_.take(6)) should be(Seq.fill(14)(expectedLeft))

        prefsTopicsExclusive(0).drop(6).map(_.drop(6)) should be(Seq.tabulate(14) { id => Seq.fill(id + 1)(Score.Zero) })
        prefsTopicsExclusive(1).drop(6).map(_.drop(6)) should be(
          Seq(
            Seq.fill(1)(Score.Zero),
            Seq.fill(2)(Score.Zero),
            Seq.fill(3)(Score.Zero),
            Seq.fill(4)(Score.Zero),
            Seq.fill(5)(Score.Zero),
            Seq.fill(6)(Score.Zero),
            Seq.fill(4)(Score.Zero) :+ Score.MinReward :+ Score.MinReward :+ Score.Zero,
            Seq.fill(4)(Score.Zero) :+ Score.MinReward :+ Score.MinReward :+ Score.Zero :+ Score.Zero,
            Seq.fill(9)(Score.Zero),
            Seq.fill(10)(Score.Zero),
            Seq.fill(11)(Score.Zero),
            Seq.fill(12)(Score.Zero),
            Seq.fill(13)(Score.Zero),
            Seq.fill(14)(Score.Zero)
          )
        )
        prefsTopicsExclusive(2).drop(6).map(_.drop(6)) should be(
          Seq(
            Seq.fill(1)(Score.Zero),
            Seq(Score.MinReward, Score.Zero),
            Seq(Score.MinReward, Score.MinReward, Score.Zero),
            Seq.fill(4)(Score.Zero),
            Seq.fill(5)(Score.Zero),
            Seq.fill(6)(Score.Zero),
            Seq.fill(7)(Score.Zero),
            Seq.fill(8)(Score.Zero),
            Seq.fill(9)(Score.Zero),
            Seq.fill(10)(Score.Zero),
            Seq.fill(11)(Score.Zero),
            Seq.fill(12)(Score.Zero),
            Seq.fill(13)(Score.Zero),
            Seq.fill(14)(Score.Zero)
          )
        )
      }

      "prefsTopicsLinked" in {
        val prefsTopicsLinked = transcription.preferences.prefsTopicsLinked.toSeq2
        prefsTopicsLinked.take(6) should be(Seq.tabulate(6) { id => Seq.fill(id + 1)(false) })
        prefsTopicsLinked.drop(6).map(_.take(6)) should be(Seq.fill(14)(Seq.fill(6)(false)))
        prefsTopicsLinked.drop(6).map(_.drop(6)) should be(Seq(
            Seq.fill(1)(false),
            Seq.fill(2)(false),
            Seq.fill(3)(false),
            Seq.fill(4)(false),
            Seq.fill(5)(false),
            Seq.fill(6)(false),
            true +: Seq.fill(6)(false),
            true +: Seq.fill(5)(false) :+ true :+ false,
            Seq.fill(9)(false),
            Seq.fill(8)(false) :+ true :+ false,
            Seq.fill(11)(false),
            Seq.fill(10)(false) :+ true :+ false,
            Seq.fill(13)(false),
            Seq.fill(12)(false) :+ true :+ false
        ))
      }
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
