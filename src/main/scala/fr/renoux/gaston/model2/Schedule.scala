package fr.renoux.gaston.model2

// TODO for all of those, check if having an Array isn't better than having a SmallIdSet
class Schedule(
    val planning: IdMap[SlotId, SmallIdSet[TopicId]],
    val assignment: IdMap[PersonId, SmallIdSet[TopicId]]
)(using
    countSlots: CountAll[SlotId],
    countTopics: CountAll[TopicId],
    countPersons: CountAll[PersonId]
) {

  lazy val topicsToPersons: IdMap[TopicId, SmallIdSet[PersonId]] = assignment.transpose

  lazy val topicsPresent: SmallIdSet[TopicId] = planning.reduceValues(_ ++ _)
}
