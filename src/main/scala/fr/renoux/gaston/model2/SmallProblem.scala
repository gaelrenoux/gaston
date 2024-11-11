package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{Count as _, *}


final class SmallProblem(
    val slotsCount: Count[SlotId],
    val slotsNames: IdMap[SlotId, String],
    val slotsPersonsPresent: IdMap[SlotId, SmallIdSet[PersonId]],
    val slotsToNextSlot: IdMap[SlotId, SlotId],
    val slotsMaxTopics: IdMap[SlotId, Count[TopicId]],

    val topicsCount: Count[TopicId],
    val topicsNames: IdMap[TopicId, String],
    val topicsMandatories: IdMap[TopicId, Array[PersonId]], // Cannot be flattened as size is not always the same
    val topicsMin: IdMap[TopicId, Count[TopicId]],
    val topicsMax: IdMap[TopicId, Count[TopicId]],
    val topicsAllowedSlot: IdMap[TopicId, SmallIdSet[SlotId]],
    val topicsFollowup: IdMap[TopicId, TopicId],
    val topicsForced: IdSet[TopicId],
    val topicsUnassignedCount: Count[TopicId],
    val topicsSimultaneous: IdMap[TopicId, SmallIdSet[TopicId]],
    val topicsNotSimultaneous: IdMap[TopicId, SmallIdSet[TopicId]],

    val personsCount: Count[PersonId],
    val personsNames: IdMap[PersonId, String],
    val personsWeight: IdMap[PersonId, Weight],
    val personsStartingScore: IdMap[PersonId, Score],

    val prefsPersonTopic: Matrix[PersonId, TopicId, Score], // also includes forbidden topics
    val prefsPersonPerson: Matrix[PersonId, PersonId, Score],
    val prefsTopicPure: IdMap[TopicId, Score], // score added for simply having this topic on schedule
    val prefsTopicsExclusive: Matrix[TopicId, TopicId, Score], // prefered to having a set of exclusive topics, to avoid an if (helps branch prediction) => CHECK
    val prefsTopicsLinked: Matrix[TopicId, TopicId, Score] // prefered to having a set of exclusive topics, to avoid an if (helps branch prediction) => CHECK
) {

  // TODO inline this maybe ?
  def scorePersons(schedule: Schedule): IdMap[PersonId, Score] = {
    schedule.content.scoreSumLines { (pid: PersonId, tid: TopicId) =>
      val topicsScore = prefsPersonTopic(pid, tid)(personsCount)
      val otherPersons = schedule.personGroups(pid)
      val otherPersonsScore = otherPersons.foldLeft(Score.Zero)(_ + prefsPersonPerson(pid, _)(personsCount))
      topicsScore + otherPersonsScore
    }(personsCount, slotsCount)
  }

  def score(schedule: Schedule): Score = {
    val personalScores: IdMap[PersonId, Score] = scorePersons(schedule)
    val personalScoresTotal = personalScores.sortedValues.fastFoldRight(Score.Zero) { (score, acc) =>
      (SmallProblem.RankFactor * acc: Score) + score
    }
    val topicsPureTotal = schedule.topicsPresent.foldLeft(Score.Zero)(_ + prefsTopicPure(_))
    // TODO missing the other non-personal scores
    personalScoresTotal + topicsPureTotal
  }

}

object SmallProblem {
  val RankFactor: Weight = Weight(0.5) // TODO move this to an appropriate class, maybe a Constants object ?
}
