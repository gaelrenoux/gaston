package fr.renoux.gaston.model2

class Schedule(
    val content: Matrix[PersonId, SlotId, TopicId]
) {
  lazy val personGroups: IdMap[PersonId, SmallIdSet[PersonId]] = ???
  lazy val topicsPresent: SmallIdSet[TopicId] = ???

}
