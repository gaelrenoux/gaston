package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Schedule, Slot, Topic}

import scala.util.Random

/**
  * Uses backtracking to create a solution satisfying all constraints. Does not take preferences into account.
  */
class ConstrainedScheduleFactory(val problem: Problem) {

  private val log = Logger[ConstrainedScheduleFactory]

  /** Returns a Schedule (if we can) satisfying all constraints), based on given random. */
  def makePartialSchedule(implicit random: Random): Option[Schedule] = {
    val slots = random.shuffle(problem.slots.toList)
    val topics = random.shuffle(problem.topics.toList)
    backtrackForConstraints(Schedule())(slots, 0, topics)
  }

  /**
    *
    * @param partialSchedule        Partial schedule we are starting from
    * @param slotsLeft              All slots we haven't handled yet
    * @param currentSlotTopicsCount Number of topics in the current slot (the head of topicsLeft)
    * @param topicsLeft             Topics we have to handle. The head is the current topic.
    * @param topicsPassed           Topics we have to handle but were already deemed unacceptable for the current solution
    * @return Some schedule that fits.
    */
  private def backtrackForConstraints(partialSchedule: Schedule)
                                     (slotsLeft: List[Slot], currentSlotTopicsCount: Int, topicsLeft: List[Topic], topicsPassed: List[Topic] = Nil): Option[Schedule] =

  if (topicsLeft.isEmpty && topicsPassed.isEmpty) {
    log.debug("Finishing backtrackForConstraints because we have no more topics")
    Some(partialSchedule) // we have integrated all topics
  } else if (topicsLeft.isEmpty) {
    log.debug("Hit a dead end")
    None // some topics were deemed unacceptable, and we have no more topics left : no solution
  } else if (slotsLeft.isEmpty) {
    log.debug("Finishing backtrackForConstraints because we have no more slots")
    Some(partialSchedule) // some topics are left but we have no more slots
  } else {
    val currentSlot = slotsLeft.head
    val currentTopic = topicsLeft.head
    val nextTopics = topicsLeft.tail

    val triplet = (currentSlot, currentTopic, problem.mandatoryPersonsPerTopic(currentTopic)) //new triplet we want to try
    val candidate = partialSchedule.copy(triplets = partialSchedule.triplets + triplet) // generate a new candidate with this triplet
    val candidateAcceptable = problem.constraints forall { c => !c.isApplicableToPartialSolution || c.isRespected(candidate) }

    val solution =
      if (candidateAcceptable) {
        log.trace(s"Candidate is acceptable: $candidate")
        val newSlotTopicsCount = (currentSlotTopicsCount + 1) % problem.parallelization // add one topic to current slot, go back to zero if we reach limit
        val newSlotsLeft = if (newSlotTopicsCount == 0) slotsLeft.tail else slotsLeft // if limit was reached, go to next slot

        /* If going to next slot, time to integrate all topics passed back. If not, there's no way it's going to work better with one more topic (schedule is even more constrained). */
        //val (newTopicsLeft, newTopicsPassed) = if (newSlotTopicsCount == 0) (topicsPassed ::: nextTopics, Nil) else (nextTopics, topicsPassed)

        /* we have a new candidate, we go on */
        //backtrackForConstraints(candidate)(newSlotsLeft, newSlotTopicsCount, newTopicsLeft, newTopicsPassed)
        backtrackForConstraints(candidate)(newSlotsLeft, newSlotTopicsCount, topicsPassed ::: nextTopics)

      } else {
        log.trace(s"Candidate is not acceptable: $candidate")
        None
      }

    solution orElse {
      log.trace(s"Backtracking from: $candidate")
      /* candidate was not acceptable, or went into a dead end. Let's try with next topic */
      backtrackForConstraints(partialSchedule)(slotsLeft, currentSlotTopicsCount, nextTopics, currentTopic :: topicsPassed)
    }
  }


}
