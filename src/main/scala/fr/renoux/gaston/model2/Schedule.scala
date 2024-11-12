package fr.renoux.gaston.model2

class Schedule(
    val content: Matrix3[SlotId, TopicId, PersonId, Boolean]
) {
  lazy val personTopics: IdMap[PersonId, SmallIdSet[TopicId]] = ???
  lazy val topicsToPersons: IdMap[TopicId, SmallIdSet[PersonId]] = ???
  lazy val topicsPresent: SmallIdSet[TopicId] = ???

}
