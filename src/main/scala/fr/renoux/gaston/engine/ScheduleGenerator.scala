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

    val initialState = Option(State(Schedule.empty, Queue(slots: _*), topics))
    Stream.iterate(initialState) {
      case Some(s) => backtrackAssignTopicsToSlots(s)
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
    val state = State(partialSchedule, slotsLeft, topicsLeft, topicsPassed)
    backtrackAssignTopicsToSlots(state) match {
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
    * @return The state of the backtracking, with a schedule that fits.
    */
  private def backtrackAssignTopicsToSlots(state: State): Option[State] = {
    if (state.isSlotsEmpty) {
      log.debug("Found an initial schedule")
      state.some

    } else if (state.isTopicsEmptyOnHeadSlot) {
      /* No topic left available for the current slot. Fail if the current slot is not satisfied yet. */
      if (state.isHeadSlotMaxPersonsTooLow) {
        log.trace("Fail because no topic available for current slot and it is not satisfied yet")
        None
      } else {
        log.trace("Go on without current slot because no topic available for it and it is already satisfied")
        backtrackAssignTopicsToSlots(state.withHeadSlotSatisfied)
      }

    } else if (state.isMaxParallelizationReachedOnHeadSlot) {
      /* Current slot has reached max parallelization. Fail if the current slot is not satisfied yet. */
      if (state.isHeadSlotMaxPersonsTooLow) {
        log.trace("Fail because current slot has reached max parallelization and it is not satisfied yet")
        None
      } else {
        /* go on without the current slot */
        log.trace("Go on without current slot because it has reached max parallelization and it is satisfied")
        backtrackAssignTopicsToSlots(state.withHeadSlotSatisfied)
      }

    } else {
      /* We can try to add more topics to the current slot */
      val candidateSchedule = state.candidate.partialSchedule

      val possibleSchedule =
        if (candidateSchedule.isSound && candidateSchedule.isPartialSolution && !candidateSchedule.on(state.headSlot).isMinPersonsTooHigh) {
          log.trace(s"Go on with acceptable candidate and next slot: $candidateSchedule")
          backtrackAssignTopicsToSlots(state.candidate)
        } else None

      possibleSchedule.orElse {
        /* candidate is not acceptable or lead to a failure, backtrack and try again with next topic */
        log.trace(s"Go on with new topic for current slot as the candidate is not OK")
        backtrackAssignTopicsToSlots(state.withPassedHeadTopic)
      }

    }
  }

}

object ScheduleGenerator {

  /** Backtracking state
    * @param partialSchedule Partial schedule we are starting from
    * @param slotsLeft All slots on which we can still add some stuff, ordered by priority (we do a round-robin). Head is the current slot.
    * @param topicsLeft Topics we can try for the current slot.
    * @param topicsPassed Topics that won't work for the current slot, but may work for ulterior slots.
    */
  private case class State(partialSchedule: Schedule, slotsLeft: Queue[Slot], topicsLeft: List[Topic], topicsPassed: List[Topic] = Nil) {

    private val problem = partialSchedule.problem

    lazy val isSlotsEmpty: Boolean = slotsLeft.isEmpty

    lazy val isTopicsEmptyOnHeadSlot: Boolean = topicsLeft.isEmpty

    lazy val isHeadSlotMaxPersonsTooLow: Boolean = partialSchedule.on(headSlot).isMaxPersonsTooLow

    lazy val isMaxParallelizationReachedOnHeadSlot: Boolean =
      ^(partialSchedule.countTopicsPerSlot.get(headSlot), problem.maxTopicCountPerSlot.get(headSlot))(_ >= _).getOrElse(false)

    lazy val headSlot: Slot = slotsLeft.head

    lazy val headTopic: Topic = topicsLeft.head

    lazy val nextRecord = Record(headSlot, headTopic, headTopic.mandatory)

    lazy val withHeadSlotSatisfied: State = State(partialSchedule, slotsLeft.tail, topicsLeft ::: topicsPassed)

    lazy val withPassedHeadTopic: State = State(partialSchedule, slotsLeft, topicsLeft.tail, headTopic :: topicsPassed)

    def withNewScheduleFromHeadTopic(schedule: Schedule): State = State(schedule, slotsLeft.tail :+ headSlot, topicsLeft.tail ::: topicsPassed)

    lazy val candidate: State = {
      val record = Record(headSlot, headTopic, headTopic.mandatory)
      val additionalTopics = problem.simultaneousTopicPerTopic(headTopic)
      val additionalRecords = additionalTopics.map(t => Record(headSlot, t, t.mandatory))
      val candidate = partialSchedule + record ++ additionalRecords

      val newTopicsLeft =
        if (additionalTopics.isEmpty) {
          topicsLeft.tail ::: topicsPassed
        } else {
          topicsLeft.tail.filter(additionalTopics.contains) ::: topicsPassed.filter(additionalTopics.contains)
        }

      State(candidate, slotsLeft.tail :+ headSlot, newTopicsLeft)
    }
  }

}