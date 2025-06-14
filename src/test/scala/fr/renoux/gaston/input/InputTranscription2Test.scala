package fr.renoux.gaston.input

import fr.renoux.gaston.TestBase
import fr.renoux.gaston.TestUtils.*
import fr.renoux.gaston.model2.*
import fr.renoux.gaston.util.{Count as _, *}


class InputTranscription2Test extends TestBase {

  val maxCount = Count.maxCount[Id].value
  val emptyIdSet = SmallIdSet.empty[Id]
  val fullIdSet = SmallIdSet.full[Id]

  // TODO Add tests for bad model

  "Correct model" - {
    val input: InputModel = InputLoader.fromClassPath("transcription-test.conf").force
    val transcription = InputTranscription2(input)
    val defMaxPersonsPerTopic = transcription.settings.defaultMaxPersonsPerTopic
    val defMinPersonsPerTopic = transcription.settings.defaultMinPersonsPerTopic
    val defMaxTopicsPerSlot = transcription.settings.defaultMaxTopicsPerSlot.get

    transcription.topics.topicsName.valuesSeq should be(
      Seq("D1-afternoon", "D1-evening", "D2-afternoon", "D3-afternoon", "D3-evening", "D3-night").map(s =>
        s"@Unassigned ($s)"
      ) ++
        Seq("Alpha", "Beta", "Gamma", "Delta") ++
        Seq("Epsilon #1", "Epsilon #2", "Eta ~1", "Eta ~2") ++
        Seq("Theta #1 ~1", "Theta #1 ~2", "Theta #2 ~1", "Theta #2 ~2", "Theta #3 ~1", "Theta #3 ~2")
    )

    object ExpectedTopics {
      val Seq(
      unassignedD1a, unassignedD1e, unassignedD2a, unassignedD3a, unassignedD3e, unassignedD3n, // 0 to 5
      alpha, beta, gamma, delta, // 6 to 9
      epsilon1, epsilon2, eta1, eta2, // 10 to 13
      theta11, theta12, theta21, theta22, theta31, theta32 // 14 to 19
      ) = (0 until 20)
    }

    "errors" in {
      transcription.errors should be(Set())
    }

    "settings" in {
      defMaxPersonsPerTopic should be(6)
      defMinPersonsPerTopic should be(4)
      defMaxTopicsPerSlot should be(50)

      transcription.settingsUnassignedPrefByPerson.valuesSeq.map(_.value.round) should be(Seq(-100, -1, -72))
    }

    "slots" in {
      import transcription.given

      transcription.slotsCount should be(6)
      transcription.slotsNames.valuesSeq should be(
        Seq("D1-afternoon", "D1-evening", "D2-afternoon", "D3-afternoon", "D3-evening", "D3-night")
      )
      transcription.slotsMaxTopics.valuesSeq should be(
        Seq(defMaxTopicsPerSlot, 5, defMaxTopicsPerSlot, defMaxTopicsPerSlot, defMaxTopicsPerSlot, defMaxTopicsPerSlot)
      )
      transcription.slotsToNextSlot.valuesSeq should be(Seq(1, SlotId.None, SlotId.None, 4, 5, SlotId.None))
      transcription.slotsPersonsPresent.valuesSeq.map(_.toSet) should be(Seq(
        Set(0, 1, 2),
        Set(0, 1, 2),
        Set(1, 2),
        Set(0, 1, 2),
        Set(0, 1, 2),
        Set(1, 2)
      ))
    }

    "persons" in {
      transcription.personsCount should be(3)
      transcription.personsName.valuesSeq should be(Seq("Albert", "Bianca", "Charly"))
      transcription.personsWeight.valuesSeq should be(Seq(Weight.Default, 1.0, 2.0))
      transcription.personsBaseScore.valuesSeq should be(Seq(Score.Zero, 20, Score.Zero))
    }

    "topics" in {
      import transcription.given

      transcription.topicsCount should be(6 + 14) // unassigned + normal (with occ/long)
      transcription.topics.topicsName.valuesSeq should be(
        Seq("D1-afternoon", "D1-evening", "D2-afternoon", "D3-afternoon", "D3-evening", "D3-night").map(s =>
          s"@Unassigned ($s)"
        ) ++
          Seq("Alpha", "Beta", "Gamma", "Delta") ++
          Seq("Epsilon #1", "Epsilon #2", "Eta ~1", "Eta ~2") ++
          Seq("Theta #1 ~1", "Theta #1 ~2", "Theta #2 ~1", "Theta #2 ~2", "Theta #3 ~1", "Theta #3 ~2")
      )
      transcription.topics.topicsForced.toSet should be((0 to 5).toSet ++ Set(8) ++ (14 to 19).toSet)
      transcription.topics.topicsMax.valuesSeq should be(
        Seq.fill(6)(maxCount) ++
          Seq(defMaxPersonsPerTopic, 5, defMaxPersonsPerTopic, defMaxPersonsPerTopic) ++
          Seq(defMaxPersonsPerTopic, defMaxPersonsPerTopic, defMaxPersonsPerTopic, defMaxPersonsPerTopic) ++
          Seq.fill(6)(7)
      )
      transcription.topics.topicsMin.valuesSeq should be(
        Seq.fill(6)(0) ++
          Seq(defMinPersonsPerTopic, defMinPersonsPerTopic, 2, defMinPersonsPerTopic) ++
          Seq(defMinPersonsPerTopic, defMinPersonsPerTopic, defMinPersonsPerTopic, defMinPersonsPerTopic) ++
          Seq.fill(6)(5)
      )
      transcription.topics.topicsMandatories.valuesSeq should be(
        Seq.fill(6)(emptyIdSet) ++
          Seq(SmallIdSet(0, 1), emptyIdSet, emptyIdSet, SmallIdSet(0)) ++
          Seq(emptyIdSet, emptyIdSet, emptyIdSet, emptyIdSet) ++
          Seq.fill(6)(SmallIdSet(2))
      )
      transcription.topics.topicsAllowedSlots.valuesSeq should be(
        (0 to 5).map(SmallIdSet(_)) ++
          Seq(fullIdSet, fullIdSet, fullIdSet, SmallIdSet(1, 4)) ++
          Seq(fullIdSet, fullIdSet, fullIdSet, fullIdSet) ++
          Seq.fill(6)(fullIdSet)
      )
      transcription.topics.topicsFollowup.valuesSeq should be(
        Seq.fill(6)(TopicId.None) ++
          Seq(TopicId.None, TopicId.None, TopicId.None, TopicId.None) ++
          Seq(TopicId.None, TopicId.None, 13, TopicId.None) ++
          Seq(15, TopicId.None, 17, TopicId.None, 19, TopicId.None)
      )
    }

    "constraints" - {
      "topicsSimultaneous" in {
        transcription.constraints.topicsSimultaneous.size should be(6 + 14)
        transcription.constraints.topicsSimultaneous.valuesSeq should be(
          Seq.fill(6)(emptyIdSet) ++
            Seq(SmallIdSet(7), SmallIdSet(6), emptyIdSet, emptyIdSet) ++
            Seq(emptyIdSet, emptyIdSet, emptyIdSet, emptyIdSet) ++
            Seq.fill(6)(emptyIdSet)
        )
      }

      "topicsNotSimultaneous" in {
        transcription.constraints.topicsNotSimultaneous.valuesSeq should be(
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
        val prefsPersonTopic = transcription.preferences.prefsPersonTopic.toSeq2.mapMap(_.value.round)

        val prefsPersonTopicUnassigned = prefsPersonTopic.map(_.take(6))
        prefsPersonTopicUnassigned should be(Seq(
          Seq.fill(6)(-100),
          Seq.fill(6)(-1),
          Seq.fill(6)(-36) // weight 2, divided by 2
        ))

        val prefsPersonTopicOther = prefsPersonTopic.map(_.drop(6))
        prefsPersonTopicOther(0) should be(Seq(
          0, // Alpha
          200, // Beta
          0, // Gamma
          0, // Delta
          800, // Epsilon #1
          800, // Epsilon #2
          0, // Eta ~1
          0, // Eta ~2
          0, // Theta #1 ~1
          0, // Theta #1 ~2
          0, // Theta #2 ~1
          0, // Theta #2 ~2
          0, // Theta #3 ~1
          0, // Theta #3 ~2
        ))
        prefsPersonTopicOther(1) should be(Seq(
          750, // Alpha
          Score.MinReward, // Beta
          Score.MinReward, // Gamma
          Score.MinReward, // Delta
          Score.MinReward, // Epsilon #1
          Score.MinReward, // Epsilon #2
          250, // Eta ~1
          250, // Eta ~2
          0, // Theta #1 ~1
          0, // Theta #1 ~2
          0, // Theta #2 ~1
          0, // Theta #2 ~2
          0, // Theta #3 ~1
          0, // Theta #3 ~2
        ))
        prefsPersonTopicOther(2) should be(Seq( // weight 2
          800 / 2, // Alpha
          0, // Beta
          0, // Gamma
          0, // Delta
          0, // Epsilon #1
          0, // Epsilon #2
          Score.MinReward.value, // Eta ~1
          Score.MinReward.value, // Eta ~2
          0, // Theta #1 ~1
          0, // Theta #1 ~2
          0, // Theta #2 ~1
          0, // Theta #2 ~2
          0, // Theta #3 ~1
          0, // Theta #3 ~2
        ))
      }

      "prefsPersonPerson" in {
        transcription.preferences.prefsPersonPerson.toSeq2.mapMap(_.value.round) should be(Seq(
          Seq(Score.Zero, input.settings.incompatibilityAntiPreference.value, Score.Zero),
          Seq.fill(3)(Score.Zero),
          Seq(200 / 2, Score.Zero, Score.Zero)
        ))
      }

      "prefsTopicsExclusive" in {
        import ExpectedTopics.*
        // 6 exclusive groups: unassigned topics, epsilon occurrences, theta occurrences, first manual groups, second manual group duplicated (because it contains Epsilon)

        val exclusivities0 = transcription.preferences.prefsTopicsExclusive(0)
        exclusivities0.count should be(3) // Albert is exempted from both manual exclusives
        exclusivities0.topicsGroups.toSeq should be(Seq(
          SmallIdSet(0 until 6),
          SmallIdSet(epsilon1, epsilon2), // Epsilon
          SmallIdSet(theta11, theta21, theta31) // Theta
        ))
        exclusivities0.scores.toSeq should be(Seq(-50, Score.MinReward, Score.MinReward))

        val exclusivities1 = transcription.preferences.prefsTopicsExclusive(1)
        exclusivities1.count should be(5) // Bianca is exempted from the first manual exclusive
        exclusivities1.topicsGroups.toSeq should be(Seq(
          SmallIdSet(0 until 6),
          SmallIdSet(epsilon1, epsilon2), // Epsilon
          SmallIdSet(theta11, theta21, theta31), // Theta
          SmallIdSet(epsilon1, eta1), SmallIdSet(epsilon2, eta1) // Manual constraint
        ))
        exclusivities1.scores.toSeq should be(Seq(-50, Score.MinReward, Score.MinReward, Score.MinReward, Score.MinReward))

        val exclusivities2 = transcription.preferences.prefsTopicsExclusive(2)
        exclusivities2.count should be(3) // Charly is exempted from the second manual exclusive and is mandatory on Theta
        exclusivities2.topicsGroups.toSeq should be(Seq(
          SmallIdSet(0 until 6),
          SmallIdSet(epsilon1, epsilon2), // Epsilon
          SmallIdSet(alpha, beta, gamma) // Manual constraint
        ))
        exclusivities2.scores.toSeq should be(Seq(-25, Score.MinReward, Score.MinReward)) // Half score, weight is 2
      }

      "prefsTopicsLinked" in {
        transcription.preferences.prefsTopicsLinked.view.map(_.toSet).toSet should be(Set(
          Set(6, 12), // Alpha + Eta 1
          Set(12, 13), // Eta parts 1 and 2
          Set(14, 15), // Theta #1 parts 1 and 2
          Set(16, 17), // Theta #2 parts 1 and 2
          Set(18, 19), // Theta #3 parts 1 and 2
        ))
      }
    }
  }

  "Bad model" in {
    val input: InputModel = InputLoader.fromClassPath("transcription-test-bad.conf").force
    val transcription = InputTranscription2(input)

    transcription.errors.nonEmpty should be(true)

    transcription.errors should contain("Settings: default min persons per topic (5) is higher than default max persons per topic (3)")
    transcription.errors should contain("Settings: Min persons on unassigned (6) is higher than max persons on unassigned (2)")
    transcription.errors should contain("Duplicate slot name: D2-afternoon")
    transcription.errors should contain("Empty slot sequence at position 2")
    transcription.errors should contain("Duplicate topic name: Gamma")
    transcription.errors should contain("Topic [@Goto]: prefix @ is reserved by the software")
    transcription.errors should contain("Topic [Spaw~nish]: Name cannot contain characters '~' or '#'")
    transcription.errors should contain("Topic [Hash#Tag]: Name cannot contain characters '~' or '#'")
    transcription.errors should contain("Topic [Beta]: Min (6) is higher than max (5)")
    transcription.errors should contain("Topic [Eta]: undefined slots: [D14-morning], [D14-night]")
    transcription.errors should contain("Duplicate person name: Donald")
    transcription.errors should contain("Person [Enzo]: undefined absence slots: [D42], [D43]")
    transcription.errors should contain("Person [Enzo]: undefined mandatory topics: [Dos], [Uno]")
    transcription.errors should contain("Person [Enzo]: undefined forbidden topics: [Aleph], [Osef]")
    transcription.errors should contain("Person [Enzo]: undefined incompatible persons: [Jack], [John]")
    transcription.errors should contain("Person [Enzo]: undefined wished topics: [Biomega], [Megabio]")
    transcription.errors should contain("Person [Enzo]: undefined wished persons: [Daisy], [Diana]")
    transcription.errors should contain("Person [Albert]: topic is both mandatory and forbidden: [Delta]")
    transcription.errors should contain("Exclusive constraint: undefined topics: [Hello], [World]")
    transcription.errors should contain("Exclusive constraint: undefined exempted persons: [Gaston], [Martin]")
    transcription.errors should contain("Exclusive constraint: should contain at least two topics: [Delta]")
    transcription.errors should contain("Exclusive constraint: should contain at least two topics: None")
    transcription.errors should contain("Linked constraint: undefined topics: [Earth], [Goodbye]")
    transcription.errors should contain("Linked constraint: should contain at least two topics: [Delta]")
    transcription.errors should contain("Linked constraint: should contain at least two topics: None")
    transcription.errors should contain("Linked constraint: can't handle multi-occurrence topics: [Epsilon], [Theta]")
    transcription.errors should contain("Simultaneous constraint: undefined topics: [Dragons], [Dungeons]")
    transcription.errors should contain("Simultaneous constraint: should contain at least two topics: [Delta]")
    transcription.errors should contain("Simultaneous constraint: should contain at least two topics: None")
    transcription.errors should contain("Simultaneous constraint: can't handle long-duration topic: [Eta]")
    transcription.errors should contain("Simultaneous constraint: different occurrence counts: [Epsilon], [Gamma]")
    transcription.errors should contain("Not-simultaneous constraint: undefined topics: [Perils], [Princesses]")
    transcription.errors should contain("Not-simultaneous constraint: should contain at least two topics: [Delta]")
    transcription.errors should contain("Not-simultaneous constraint: should contain at least two topics: None")

    transcription.errors.size should be(34)

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
      println("Followups: " + transcription.slotsToNextSlot.toPrettyString)

      println("\nPersons ----------------------------------")
      println("Count: " + transcription.personsCount.toPrettyString)
      println("Names: " + transcription.personsName.toPrettyString)
      println("Weights: " + transcription.personsWeight.toPrettyString)
      println("Base scores: " + transcription.personsBaseScore.toPrettyString)

      println("\nTopics -----------------------------------")
      println("Count: " + transcription.topicsCount.toPrettyString)
      println("Names: " + transcription.topics.topicsName.toPrettyString)
      println("Max: " + transcription.topics.topicsMax.toPrettyString)
      println("Min: " + transcription.topics.topicsMin.toPrettyString)
      println("Mandatories: " + transcription.topics.topicsMandatories.toPrettyString)
    }
  }
}
