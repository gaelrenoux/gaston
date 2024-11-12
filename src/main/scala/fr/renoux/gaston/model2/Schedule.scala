package fr.renoux.gaston.model2

class Schedule(
    val content: IdMatrix3[SlotId, TopicId, PersonId, Boolean]
)(
    countSlots: Count[SlotId],
    countTopics: Count[TopicId],
    countPersons: Count[PersonId]
) {

  // TODO for all of those, check if having an Array isn't better than having a SmallIdSet
  
  lazy val personToTopics: IdMap[PersonId, SmallIdSet[TopicId]] = 
    content.listSmallTopicsByPerson(countSlots, countTopics, countPersons)

  lazy val topicsToPersons: IdMap[TopicId, SmallIdSet[PersonId]] =
    content.listSmallPersonsByTopic(countSlots, countTopics, countPersons)
  
  lazy val topicsPresent: SmallIdSet[TopicId] =
    content.listSmallTopics(countSlots, countTopics, countPersons)


}
