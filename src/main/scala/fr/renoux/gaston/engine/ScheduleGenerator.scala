package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.ScheduleGenerator.BacktrackingFailures
import fr.renoux.gaston.model._
import fr.renoux.gaston.util.CollectionImplicits._
import scalaz.Scalaz._
import scalaz.\/

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random

/**
  * Uses backtracking to produce a Stream of schedules. Those schedules are not the best you could have, but they are
  * valid and all persons have their optimal slots.
  *
  * Placing persons on the generated schedule is delegated to the PartialScheduleFiller.
  */
class ScheduleGenerator(triggerOnFailures: BacktrackingFailures => Unit)(implicit problem: Problem, ctx: Context) {

  private val log = Logger[ScheduleGenerator]

  import ScheduleGenerator._

  private val filler = new PartialScheduleFiller
  private val improver = new PersonPlacementImprover

  /** Generates just one schedule. */
  def createOne(implicit random: Random): Schedule = {
    log.debug("Generating a single schedule")
    val slots = random.shuffle(problem.slots.toList)
    val (forcedTopics, unforcedTopics) = problem.topics.filter(_.removable).toList.partition(_.forced)
    val topics = random.shuffle(forcedTopics) ::: random.shuffle(unforcedTopics)
    val state = State(Schedule.empty, Queue(slots: _*), topics)
    val unimproved = backtrackAndFill(state)
    improver.improve(unimproved.get)
  }

  @tailrec
  private def backtrackAndFill(state: State)
    (implicit random: Random): Option[Schedule] = {
    backtrackAssignTopicsToSlots(state).toOption match {
      case None => None
      case Some(newState) => filler.fill(newState.partialSchedule) match {
        case None => backtrackAndFill(newState)
        case Some(schedule) => Some(schedule)
      }
    }
  }

  /**
    * Uses backtracking to construct a partial schedule with all topics assigned to slots, and mandatory people assigned
    * to their topics. Returns the current backtracking state (including the schedule), so that we may start again if
    * needed.
    * @return The state of the backtracking, with a schedule that fits.
    */
  // scalastyle:off method.length
  private def backtrackAssignTopicsToSlots(
      state: State,
      failures: BacktrackingFailures = BacktrackingFailures(triggerOnFailures)
  ): BacktrackingFailures \/ State = {
    if (state.isSlotsEmpty) {
      log.debug("Found an initial schedule")
      state.right[BacktrackingFailures]

    } else if (state.isTopicsEmptyOnHeadSlot) {
      /* No topic left available for the current slot. Fail if the current slot is not satisfied yet. */
      if (state.isHeadSlotMaxPersonsTooLow) {
        log.trace("Fail because no topic available for current slot and it is not satisfied yet")
        failures.addNoTopics(state.headSlot).left[State]
      } else {
        /* Go on without the current slot */
        log.trace("Go on without current slot because no topic available for it and it is already satisfied")
        backtrackAssignTopicsToSlots(state.withHeadSlotSatisfied, failures)
      }

    } else if (state.isMaxTopicsReachedOnHeadSlot) {
      /* Current slot has reached max parallelization. Fail if the current slot is not satisfied yet. */
      if (state.isHeadSlotMaxPersonsTooLow) {
        log.trace("Fail because current slot has reached max parallelization and it is not satisfied yet")
        failures.addMaxParallelizationReached(state.headSlot).left[State]
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
      backtrackAssignTopicsToSlots(state.withCandidateAcknowledged, failures).recoverWith[BacktrackingFailures, State] { case fs: BacktrackingFailures =>
        /* candidate is not acceptable or lead to a failure, backtrack and try again with next topic */
        log.trace("Go on with new topic for current slot as the candidate is not OK")
        backtrackAssignTopicsToSlots(state.withPassedHeadTopic, fs)
      }
    }
  }
  // scalastyle:on method.length

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

    lazy val isMaxTopicsReachedOnHeadSlot: Boolean = partialSchedule.countTopicsPerSlot.getOrElse(headSlot, 0) >= headSlot.maxTopics

    lazy val isHeadTopicOkOnHeadSlot: Boolean = headTopic.slots.forall(_.contains(headSlot))

    lazy val headSlot: Slot = slotsLeft.head

    lazy val headTopic: Topic = topicsLeft.head

    lazy val nextRecord = Record(headSlot, headTopic, headTopic.mandatory)

    lazy val withHeadSlotSatisfied: State = State(partialSchedule, slotsLeft.tail, topicsLeft ::: topicsPassed)

    lazy val withPassedHeadTopic: State = State(partialSchedule, slotsLeft, topicsLeft.tail, headTopic :: topicsPassed)

    private lazy val topicsSimultaneousWithHeadTopic = problem.simultaneousTopicPerTopic(headTopic)

    lazy val candidate: Schedule = {
      val record = Record(headSlot, headTopic, headTopic.mandatory)

      val additionalRecords = topicsSimultaneousWithHeadTopic.map(t => Record(headSlot, t, t.mandatory))
      partialSchedule + record ++ additionalRecords
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
      candidate.isSound && candidate.isPartialSolution && !candidate.on(headSlot).isMinPersonsTooHigh

  }

  case class BacktrackingFailures(
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
