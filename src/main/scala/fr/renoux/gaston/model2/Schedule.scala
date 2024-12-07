package fr.renoux.gaston.model2

// TODO for all of those, check if having an Array isn't better than having a SmallIdSet
class Schedule(
    val planning: IdMap[SlotId, SmallIdSet[TopicId]],
    var topicsPresent: SmallIdSet[TopicId],
    val personsToTopics: IdMap[PersonId, SmallIdSet[TopicId]],
    val topicsToPersons: IdMap[TopicId, SmallIdSet[PersonId]]
)(using
    val countSlots: CountAll[SlotId],
    val countTopics: CountAll[TopicId],
    val countPersons: CountAll[PersonId]
) {

  inline def move(pid: PersonId, tid1: TopicId, tid2: TopicId): Schedule = {
    // TODO Dev mode control: both topics should be on the same slot, pid should be on tid1
    personsToTopics(pid) = personsToTopics(pid) - tid1 + tid2
    topicsToPersons(tid1) = topicsToPersons(tid1) - pid
    topicsToPersons(tid2) = topicsToPersons(tid2) + pid
    this
  }

  def reverseMove(pid: PersonId, tid1: TopicId, tid2: TopicId): Schedule = move(pid, tid2, tid1)

  inline def addTopic(sid: SlotId, tid: TopicId): Schedule = {
    planning(sid) = planning(sid) + tid
    topicsPresent = topicsPresent + tid
    this
  }

  inline def removeTopic(sid: SlotId, tid: TopicId): Schedule = {
    planning(sid) = planning(sid) - tid
    topicsPresent = topicsPresent - tid
    this
  }

  def isValidFor(pb: SmallProblem): Boolean = {
    // TODO Will be helpful in tests
    ???
  }
}

object Schedule {
  def from(
      planning: IdMap[SlotId, SmallIdSet[TopicId]],
      assignment: IdMap[PersonId, SmallIdSet[TopicId]]
  )(using
      countSlots: CountAll[SlotId],
      countTopics: CountAll[TopicId],
      countPersons: CountAll[PersonId]
  ) = {
    Schedule(
      planning,
      planning.reduceValues(_ ++ _),
      assignment,
      assignment.transpose
    )
  }
}
