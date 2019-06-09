package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model._
import scalaz.Scalaz._

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random

/**
  * Uses backtracking to produce a Stream of schedules. Those schedules are not the best you could have, but they are
  * valid and all persons have their optimal slots.
  */
class ScheduleGenerator(implicit problem: Problem, ctx: Context) {

  private val log = Logger[ScheduleGenerator]

  import ScheduleGenerator._

  private val filler = new PartialScheduleFiller
  private val improver = new PersonPlacementImprover

  /** A lazy sequence of partial schedules. If the first one doesn't fit, go on. Ends when we can't backtrack any more. */
  def lazySeq(implicit random: Random): Stream[Schedule] = {
    log.debug("Generating a stream of schedules")
    val slots = random.shuffle(problem.slots.toList)
    val topics = random.shuffle(problem.topics.toList)

    val initialState = backtrackAssignTopicsToSlots(Schedule.empty)(Queue(slots: _*), topics)
    Stream.iterate(initialState) {
      case Some(State(ps, sl, tl, tp)) => backtrackAssignTopicsToSlots(ps)(sl, tl, tp)
      case None => None
    }.takeWhile(_.isDefined).map(_.get.partialSchedule)
  }

  /** Generates just one schedule. */
  def createOne(implicit random: Random): Schedule = {
    log.debug("Generating a single schedule")
    val slots = random.shuffle(problem.slots.toList)
    val topics = random.shuffle(problem.topics.toList.filter(_.removable))

    val unimproved = recFill(Schedule.empty, Queue(slots: _*), topics, Nil)
    improver.improve(unimproved.get)
  }

  @tailrec
  private def recFill(partialSchedule: Schedule, slotsLeft: Queue[Slot], topicsLeft: List[Topic], topicsPassed: List[Topic])
    (implicit random: Random): Option[Schedule] = {
    backtrackAssignTopicsToSlots(partialSchedule)(slotsLeft, topicsLeft, topicsPassed) match {
      case None => None
      case Some(State(ps, sl, tl, tp)) => filler.fill(ps) match {
        case None => recFill(ps, sl, tl, tp)
        case Some(s) => Some(s)
      }
    }
  }

  /**
    * Uses backtracking to construct a partial schedule with all topics assigned to slots, and mandatory people assigned
    * to their topics. Returns the current backtracking state (including the schedule), so that we may start again if
    * needed.
    * @param partialSchedule Partial schedule we are starting from
    * @param slotsLeft All slots on which we can still add some stuff, ordered by priority (we do a round-robin). Head is the current slot.
    * @param topicsLeft Topics we can try for the current slot.
    * @param topicsPassed Topics that won't work for the current slot, but may work for ulterior slots.
    * @return The state of the backtracking, with a schedule that fits.
    */
  private def backtrackAssignTopicsToSlots(partialSchedule: Schedule)
    (slotsLeft: Queue[Slot], topicsLeft: List[Topic], topicsPassed: List[Topic] = Nil): Option[State] = {

    if (slotsLeft.isEmpty) {
      log.debug("Found an initial schedule")
      State(partialSchedule, slotsLeft, topicsLeft, topicsPassed).some

    } else if (topicsLeft.isEmpty) {
      /* No topic available for the current slot. If the current slot is not satisfied, we fail because there is no way to satisfy the current slot at this point. */
      if (maxPersonsOnSlot(partialSchedule, slotsLeft.head) < problem.personsCount) {
        log.trace("Fail because no topic available for current slot and it is not satisfied yet")
        None
      } else {
        /* go on without the current slot */
        log.trace("Go on without current slot because no topic available for it and it is already satisfied")
        backtrackAssignTopicsToSlots(partialSchedule)(slotsLeft.tail, topicsLeft ::: topicsPassed)
      }

    } else {
      val currentSlot = slotsLeft.head
      val maxTopicCount = problem.maxTopicCountPerSlot.get(currentSlot)

      if (maxTopicCount.exists(topicCountOnSlot(partialSchedule, currentSlot) >= _)) {
        /* The current slot has reached max parallelization. If it is not satisfied, we fail because we can't add more topics */
        if (maxPersonsOnSlot(partialSchedule, currentSlot) < problem.personsCount) {
          log.trace("Fail because current slot has reached max parallelization and it is not satisfied yet")
          None
        } else {
          /* go on without the current slot */
          log.trace("Go on without current slot because it has reached max parallelization and it is satisfied")
          backtrackAssignTopicsToSlots(partialSchedule)(slotsLeft.tail, topicsLeft ::: topicsPassed)
        }
      } else {
        /* We can try to add more topics to the current slot */
        val currentTopic = topicsLeft.head
        val nextTopics = topicsLeft.tail

        val record = Record(currentSlot, currentTopic, currentTopic.mandatory) //new record we want to try
        val necessaryAdditions = problem.simultaneousTopicPerTopic(currentTopic).map(t => Record(currentSlot, t, t.mandatory))

        val candidate = partialSchedule + record ++ necessaryAdditions // generate a new candidate with this record

        val possibleSchedule =
          if (candidate.isSound && candidate.isPartialSolution && minPersonsOnSlot(candidate, currentSlot) <= problem.personsCount) {
            log.trace(s"Go on with acceptable candidate and next slot: $candidate")
            backtrackAssignTopicsToSlots(candidate)(slotsLeft.tail :+ currentSlot, nextTopics ::: topicsPassed, Nil)
          } else None

        possibleSchedule.orElse {
          /* candidate is not acceptable or lead to a failure, try again with next topic */
          log.trace(s"Go on with new topic for current slot as the candidate is not OK")
          backtrackAssignTopicsToSlots(partialSchedule)(slotsLeft, nextTopics, currentTopic :: topicsPassed)
        }

      }
    }
  }

  private def topicCountOnSlot(schedule: Schedule, slot: Slot) = schedule.topicsPerSlot.get(slot).map(_.size).getOrElse(0)

  private def minPersonsOnSlot(schedule: Schedule, slot: Slot) = schedule.minPersonsOnSlot.getOrElse(slot, 0)

  private def maxPersonsOnSlot(schedule: Schedule, slot: Slot) = schedule.maxPersonsOnSlot.getOrElse(slot, 0)

}

object ScheduleGenerator {


  /** Backtracking state
    * @param partialSchedule Partial schedule we are starting from
    * @param slotsLeft All slots on which we can still add some stuff, ordered by priority (we do a round-robin). Head is the current slot.
    * @param topicsLeft Topics we can try for the current slot.
    * @param topicsPassed Topics that won't work for the current slot, but may work for ulterior slots.
    */
  private case class State(partialSchedule: Schedule, slotsLeft: Queue[Slot], topicsLeft: List[Topic], topicsPassed: List[Topic] = Nil)

}