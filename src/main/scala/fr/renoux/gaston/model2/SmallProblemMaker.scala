package fr.renoux.gaston.model2

import fr.renoux.gaston.util.testOnly

/** Creates an extremely simple problem, with dummy names, given a count of the various required entities. Used in tests. */
@testOnly
object SmallProblemMaker {


  def make(using countSlots: CountAll[SlotId], countTopics: CountAll[TopicId], countPersons: CountAll[PersonId]): SmallProblem = {
    SmallProblem(
      slotsCount = countSlots,
      slotsToName = IdMap.tabulate[SlotId, String] { sid => s"Slot #$sid" },
      slotsToPersonsPresent = IdMap.fill[SlotId, SmallIdSet[PersonId]](SmallIdSet.full[PersonId]),
      slotsToNextSlot = IdMap.fill[SlotId, SlotId](SlotId.None),
      slotsToMaxTopics = IdMap.fill[SlotId, Count[TopicId]](countTopics),

      topicsCount = countTopics,
      topicsToName = IdMap.tabulate[TopicId, String] { tid => s"Topic #$tid" },
      topicsToMandatories = IdMap.fill(SmallIdSet.empty[PersonId]),
      topicsToForbiddens = IdMap.fill(SmallIdSet.empty[PersonId]),
      topicsToMinPersons = IdMap.fill[TopicId, Count[PersonId]](0),
      topicsToMaxPersons = IdMap.fill[TopicId, Count[PersonId]](countPersons),
      topicsToAllowedSlots = IdMap.fill(SmallIdSet.empty[SlotId]),
      topicsToFollowup = IdMap.fill(TopicId.None),
      topicsForced = SmallIdSet.empty,
      topicsToSimultaneous = IdMap.fill(SmallIdSet.empty[TopicId]),
      topicsToNotSimultaneous = IdMap.fill(SmallIdSet.empty[TopicId]),

      personsCount = countPersons,
      personsToName = IdMap.tabulate[PersonId, String] { pid => s"Person #$pid" },
      personsToWeight = IdMap.fill[PersonId, Weight](Weight.Default),
      personsToBaseScore = IdMap.fill[PersonId, Score](Score.Zero),

      prefsPersonTopic = IdMatrix.fill[PersonId, TopicId, Score](Score.Zero),
      prefsPersonPerson = IdMatrix.fill[PersonId, PersonId, Score](Score.Zero),
      prefsTopicPure = IdMap.fill[TopicId, Score](Score.Zero),
      prefsTopicsExclusive = IdMap.fill[PersonId, Exclusivities](Exclusivities.empty),

      prefsTopicsLinked = Array.empty
    )

  }
}
