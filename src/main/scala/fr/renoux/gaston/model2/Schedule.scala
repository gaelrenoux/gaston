package fr.renoux.gaston.model2

class Schedule(
    val content: IdMatrix3[SlotId, TopicId, PersonId, Boolean]
)(using
    countSlots: CountAll[SlotId],
    countTopics: CountAll[TopicId],
    countPersons: CountAll[PersonId]
) {

  // TODO for all of those, check if having an Array isn't better than having a SmallIdSet
  
  lazy val personToTopics: IdMap[PersonId, SmallIdSet[TopicId]] = 
    content.listSmallTopicsByPerson

  lazy val topicsToPersons: IdMap[TopicId, SmallIdSet[PersonId]] =
    content.listSmallPersonsByTopic
  
  lazy val topicsPresent: SmallIdSet[TopicId] =
    content.listSmallTopics


}
