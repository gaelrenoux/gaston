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

  // TODO Instead of transposing, probably better to update both this and assignment every time
  lazy val topicsToPersons: IdMap[TopicId, SmallIdSet[PersonId]] = assignment.transpose

  lazy val topicsPresent: SmallIdSet[TopicId] = planning.reduceValues(_ ++ _)

  inline def move(pid: PersonId, tid1: TopicId, tid2: TopicId): Schedule = {
    // TODO Dev mode control: both topics should be on the same slot, pid should be on tid1
    assignment(pid) = assignment(pid) - tid1 + tid2
    this
  }

  def reverseMove(pid: PersonId, tid1: TopicId, tid2: TopicId): Schedule = move(pid, tid2, tid1)

  inline def addTopic(sid: SlotId, tid: TopicId): Schedule = {
    planning(sid) = planning(sid) + tid
    this
  }

  inline def removeTopic(sid: SlotId, tid: TopicId): Schedule = {
    planning(sid) = planning(sid) - tid
    this
  }
}
