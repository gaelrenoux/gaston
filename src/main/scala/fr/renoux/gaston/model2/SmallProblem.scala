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
    val topicsMandatories: IdMap[TopicId, SmallIdSet[PersonId]],
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
    val prefsTopicsExclusive: Array[Array[TopicId]], // a person cannot be on multiple exclusive topics
    val prefsTopicsLinked: Array[Array[TopicId]] // a person must be either on all linked topics, or on none of them
) {

  // TODO inline this maybe ?
  def scorePersons(schedule: Schedule): IdMap[PersonId, Score] = {
    schedule.personToTopics.mapToScore { (pid, topicIds) =>
      topicIds.mapSumToScore { tid =>
        val topicsScore = prefsPersonTopic(pid, tid)(topicsCount)
        val otherPersons = schedule.topicsToPersons(tid).removed(pid)
        val otherPersonsScore = otherPersons.mapSumToScore(prefsPersonPerson(pid, _)(personsCount))
        topicsScore + otherPersonsScore
      }
    }
  }

  def scoreExclusive(schedule: Schedule): Score = {
    var score = Score.Zero
    prefsTopicsExclusive.fastForeach { exclusives => 
      var i = 0
      while (i + 1 < exclusives.length) {
        val topic1 = exclusives(i)
        var j = i + 1
        val persons1 = schedule.topicsToPersons(topic1)
        while (j < exclusives.length) {
          val topic2 = exclusives(j)
          val persons2 = schedule.topicsToPersons(topic2)
          val intersection = persons1 && persons2 &&! topicsMandatories(topic1)
          if (intersection.nonEmpty) {
            score += Score.MinReward
          }
          j += 1
        }
        i +=1
      }
    }
    score
  }

  def scoreLinked(schedule: Schedule): Score = {
    var score = Score.Zero
    prefsTopicsLinked.fastForeach { linked => 
      var i = 0
      while (i + 1 < linked.length) {
        val topic1 = linked(i)
        var j = i + 1
        val persons1 = schedule.topicsToPersons(topic1)
        while (j < linked.length) {
          val topic2 = linked(j)
          val persons2 = schedule.topicsToPersons(topic2)
          if (persons1 != persons2) {
            score += Score.MinReward
          }
          j += 1
        }
        i +=1
      }
    }
    score
  }

  def score(schedule: Schedule): Score = {
    val personalScores: IdMap[PersonId, Score] = scorePersons(schedule)
    val personalScoresTotal = personalScores.sortedValues.fastFoldRight(Score.Zero) { (score, acc) =>
      (SmallProblem.RankFactor * acc: Score) + score
    }
    val topicsPureTotal = schedule.topicsPresent.mapSumToScore(prefsTopicPure(_))
    personalScoresTotal + topicsPureTotal + scoreExclusive(schedule) + scoreLinked(schedule)
  }

}

object SmallProblem {
  val RankFactor: Weight = 0.5 // TODO move this to an appropriate class, maybe a Constants object ?
}
