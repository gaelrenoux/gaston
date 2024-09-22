package fr.renoux.gaston.engine

import cats.implicits._
import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.RandomScheduleGenerator.BacktrackingFailures
import fr.renoux.gaston.engine.assignment.{AssignmentImprover, RandomAssigner}
import fr.renoux.gaston.model._
import fr.renoux.gaston.util.CollectionImplicits._
import fr.renoux.gaston.util.Context

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random

/**
  * Uses backtracking to produce a valid schedule, making random choices to provide a planning. Given the planning,
  * assignment of persons is optimized.
  */
final class RandomScheduleGenerator(triggerOnFailures: BacktrackingFailures => Unit)(implicit problem: Problem, ctx: Context) {

  private val log = Logger[RandomScheduleGenerator]

  import RandomScheduleGenerator._

  private val randomAssigner = new RandomAssigner
  private val improver = new AssignmentImprover

  // TODO Handle followup topics in this class
  if (problem.topicsList.exists(_.followup.nonEmpty)) {
    throw new UnsupportedOperationException("The RandomScheduleGenerator doesn't support followup topics yet")
  }

  /** Generates just one schedule. */
  def create(chainSeed: Long)(implicit random: Random): Schedule = {
    log.debug("Generating a single schedule")
    val slots = random.shuffle(problem.slotsList)
    // TODO need to handle followup topics !
    val (forcedTopics, unforcedTopics) = problem.topicsList.filterNot(_.isFollowup).partition(_.forced)
    val topics = random.shuffle(forcedTopics) ::: random.shuffle(unforcedTopics)
    val state = State(Schedule.empty(chainSeed), Queue(slots: _*), topics)
    val unimproved = backtrackAndFill(state)
    improver.improve(unimproved.get)
  }

  @tailrec
  private def backtrackAndFill(state: State)
    (implicit random: Random): Option[Schedule] = {
    backtrackAssignTopicsToSlots(state).toOption match {
      case None => None
      case Some(newState) => randomAssigner.fill(newState.unfilledSchedule) match {
        case None => backtrackAndFill(newState)
        case Some(schedule) => Some(schedule)
      }
    }
  }

  /**
    * Uses backtracking to construct an unfilled schedule with all topics assigned to slots, and mandatory people
    * assigned to their topics. Returns the current backtracking state (including the schedule), so that we may start
    * again if needed.
    * @return The state of the backtracking, with a schedule that fits.
    */
  // scalastyle:off method.length
  private def backtrackAssignTopicsToSlots(
      state: State,
      failures: BacktrackingFailures = BacktrackingFailures(triggerOnFailures)
  ): Either[BacktrackingFailures, State] = {
    if (state.isSlotsEmpty) {
      log.debug("Found an initial schedule")
      Right(state)

    } else if (state.isTopicsEmptyOnHeadSlot) {
      /* No topic left available for the current slot. Fail if the current slot is not satisfied yet. */
      if (state.isHeadSlotMaxPersonsTooLow) {
        log.trace("Fail because no topic available for current slot and it is not satisfied yet")
        Left(failures.addNoTopics(state.headSlot))
      } else {
        /* Go on without the current slot */
        log.trace("Go on without current slot because no topic available for it and it is already satisfied")
        backtrackAssignTopicsToSlots(state.withHeadSlotSatisfied, failures)
      }

    } else if (state.isMaxTopicsReachedOnHeadSlot) {
      /* Current slot has reached max parallelization. Fail if the current slot is not satisfied yet. */
      if (state.isHeadSlotMaxPersonsTooLow) {
        log.trace("Fail because current slot has reached max parallelization and it is not satisfied yet")
        Left(failures.addMaxParallelizationReached(state.headSlot))
      } else {
        /* Go on without the current slot */
        log.trace("Go on without current slot because it has reached max parallelization and it is satisfied")
        backtrackAssignTopicsToSlots(state.withHeadSlotSatisfied, failures)
      }

    } else if (!state.isHeadTopicOkOnHeadSlot) {
      /* current topic cannot be added to the current slot, go on to next topic */
      log.trace("Go on with new topic for current slot as the topic is not compatible with the slot")
      backtrackAssignTopicsToSlots(state.withPassedHeadTopic, failures)

    } else if (!state.isGoodCandidate) {
      /* candidate is not acceptable or lead to a failure, go on to next topic */
      log.trace("Go on with new topic for current slot as the candidate is not OK")
      backtrackAssignTopicsToSlots(state.withPassedHeadTopic, failures)

    } else {
      /* Finally ! We can try to add the current topic to the current slot */
      log.trace(s"Go on with acceptable candidate and next slot: ${state.candidate}")
      backtrackAssignTopicsToSlots(state.withCandidateAcknowledged, failures).recoverWith { case fs: BacktrackingFailures =>
        /* candidate is not acceptable or lead to a failure, backtrack and try again with next topic */
        log.trace("Go on with new topic for current slot as the candidate is not OK")
        backtrackAssignTopicsToSlots(state.withPassedHeadTopic, fs)
      }
    }
  }
  // scalastyle:on method.length

}

object RandomScheduleGenerator {

  /** Backtracking state
    * @param unfilledSchedule Unfilled schedule we are starting from
    * @param slotsLeft All slots on which we can still add some stuff, ordered by priority (we do a round-robin). Head is the current slot.
    * @param topicsLeft Topics we can try for the current slot.
    * @param topicsPassed Topics that won't work for the current slot, but may work for ulterior slots.
    */
  private final case class State(unfilledSchedule: Schedule, slotsLeft: Queue[Slot], topicsLeft: List[Topic], topicsPassed: List[Topic] = Nil) {

    private implicit val problem: Problem = unfilledSchedule.problem

    lazy val isSlotsEmpty: Boolean = slotsLeft.isEmpty

    lazy val isTopicsEmptyOnHeadSlot: Boolean = topicsLeft.isEmpty

    lazy val isHeadSlotMaxPersonsTooLow: Boolean = headSlotSchedule.isMaxPersonsTooLow

    lazy val isMaxTopicsReachedOnHeadSlot: Boolean = headSlotSchedule.maxTopicsLeft <= 0

    lazy val isHeadTopicOkOnHeadSlot: Boolean = headTopic.slots.forall(_.contains(headSlot))

    lazy val headSlot: Slot = slotsLeft.head

    lazy val headSlotSchedule: SlotSchedule = unfilledSchedule.on(headSlot)

    lazy val headTopic: Topic = topicsLeft.head

    lazy val nextRecord: Record = Record(headSlot, headTopic, headTopic.mandatory)

    lazy val withHeadSlotSatisfied: State = State(unfilledSchedule, slotsLeft.tail, topicsLeft ::: topicsPassed)

    lazy val withPassedHeadTopic: State = State(unfilledSchedule, slotsLeft, topicsLeft.tail, headTopic :: topicsPassed)

    private lazy val topicsSimultaneousWithHeadTopic = problem.simultaneousTopicByTopic(headTopic)

    lazy val candidate: Schedule = {
      val record = Record(headSlot, headTopic, headTopic.mandatory)

      val additionalRecords = topicsSimultaneousWithHeadTopic.map(t => Record(headSlot, t, t.mandatory))
      unfilledSchedule.add(record).addAll(headSlot, additionalRecords)
    }

    lazy val withCandidateAcknowledged: State = {
      val newTopicsLeft =
        if (topicsSimultaneousWithHeadTopic.isEmpty) {
          topicsPassed.reverse ::: topicsLeft.tail
        } else {
          topicsPassed.filterNot(topicsSimultaneousWithHeadTopic.contains).reverse ::: topicsLeft.tail.filterNot(topicsSimultaneousWithHeadTopic.contains)
        }

      State(candidate, slotsLeft.tail :+ headSlot, newTopicsLeft)
    }

    lazy val isGoodCandidate: Boolean =
      candidate.isSound && candidate.isUnfilledSolution && !candidate.on(headSlot).isMinPersonsTooHigh

  }

  final case class BacktrackingFailures(
      triggerOnFailures: BacktrackingFailures => Unit,
      noTopics: Map[Slot, Int] = Map(),
      maxParallelizationReached: Map[Slot, Int] = Map(),
      total: Int = 0
  ) {
    if (total > 0) triggerOnFailures(this)

    def addNoTopics(slot: Slot): BacktrackingFailures =
      copy(noTopics = noTopics.updatedWithOrElse(slot)(_ + 1, 1), total = total + 1)

    def addMaxParallelizationReached(slot: Slot): BacktrackingFailures =
      copy(maxParallelizationReached = maxParallelizationReached.updatedWithOrElse(slot)(_ + 1, 1), total = total + 1)
  }

}
