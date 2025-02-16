package fr.renoux.gaston.model2

object SmallProblemMaker {


  def make(using countSlots: CountAll[SlotId], countTopics: CountAll[TopicId], countPersons: CountAll[PersonId]): SmallProblem = {
    new SmallProblem(
      slotsCount = countSlots,
      slotsNames = IdMap.tabulate[SlotId, String] { sid => s"Slot #$sid" },
      slotsPersonsPresent = IdMap.fill[SlotId, SmallIdSet[PersonId]](SmallIdSet.full[PersonId]),
      slotsToNextSlot = IdMap.fill[SlotId, SlotId](SlotId.None),
      slotsMaxTopics = IdMap.fill[SlotId, Count[TopicId]](countTopics),

      topicsCount = countTopics,
      topicsName = IdMap.tabulate[TopicId, String] { tid => s"Topic #$tid" },
      topicsMandatories = IdMap.fill(SmallIdSet.empty[PersonId]),
      topicsMin = IdMap.fill[TopicId, Count[PersonId]](0),
      topicsMax = IdMap.fill[TopicId, Count[PersonId]](countPersons),
      topicsAllowedSlots = IdMap.fill(SmallIdSet.empty[SlotId]),
      topicsFollowup = IdMap.fill(TopicId.None),
      topicsForced = SmallIdSet.empty,
      topicsSimultaneous = IdMap.fill(SmallIdSet.empty[TopicId]),
      topicsNotSimultaneous = IdMap.fill(SmallIdSet.empty[TopicId]),

      personsCount = countPersons,
      personsName = IdMap.tabulate[PersonId, String] { pid => s"Person #$pid" },
      personsWeight = IdMap.fill[PersonId, Weight](Weight.Default),
      personsBaseScore = IdMap.fill[PersonId, Score](Score.Zero),

      prefsPersonTopic = IdMatrix.fill[PersonId, TopicId, Score](Score.Zero),
      prefsPersonPerson = IdMatrix.fill[PersonId, PersonId, Score](Score.Zero),
      prefsTopicPure = IdMap.fill[TopicId, Score](Score.Zero),
      prefsTopicsExclusive = IdMap.fill[PersonId, Exclusivities](Exclusivities.empty),

      prefsTopicsLinked = Array.empty
    )

  }
}
