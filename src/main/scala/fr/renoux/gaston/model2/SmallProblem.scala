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

    val prefsPersonTopic: IdMatrix[PersonId, TopicId, Score], // also includes forbidden topics
    val prefsPersonPerson: IdMatrix[PersonId, PersonId, Score],
    val prefsTopicPure: IdMap[TopicId, Score], // score added for simply having this topic on schedule
    val prefsTopicsExclusive: IdMatrix[TopicId, TopicId, Score], // prefered to having a set of exclusive topics, to avoid an if (helps branch prediction) => CHECK
    val prefsTopicsLinked: IdMatrix[TopicId, TopicId, Score] // prefered to having a set of exclusive topics, to avoid an if (helps branch prediction) => CHECK
) {

  // TODO inline this maybe ?
  def scorePersons(schedule: Schedule): IdMap[PersonId, Score] = {
    schedule.personToTopics.mapToScore { (pid, topicIds) =>
      topicIds.mapSumToScore { tid =>
        val topicsScore = prefsPersonTopic(pid, tid)(personsCount)
        val otherPersons = schedule.topicsToPersons(tid).removed(pid)
        val otherPersonsScore = otherPersons.mapSumToScore(prefsPersonPerson(pid, _)(personsCount))
        topicsScore <+> otherPersonsScore
      }
    }
  }

  def score(schedule: Schedule): Score = {
    val personalScores: IdMap[PersonId, Score] = scorePersons(schedule)
    val personalScoresTotal = personalScores.sortedValues.fastFoldRight(Score.Zero) { (score, acc) =>
      (SmallProblem.RankFactor <*> acc: Score) <+> score
    }
    val topicsPureTotal = schedule.topicsPresent.mapSumToScore(prefsTopicPure(_))
    // TODO missing the other non-personal scores
    personalScoresTotal <+> topicsPureTotal
  }

}

object SmallProblem {
  val RankFactor: Weight = 0.5 // TODO move this to an appropriate class, maybe a Constants object ?
}
