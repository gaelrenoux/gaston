package fr.renoux.gaston.engine

import java.security.MessageDigest

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model._
import scalaz.Scalaz._

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.util.Random

/**
  * Uses backtracking to produce a Stream of partial schedules. Those schedules do not violate any constraint, but not
  * all persons are in it.
  */
class PartialSchedulesGenerator(val problem: Problem) {

  import PartialSchedulesGenerator._

  private implicit val _p: Problem = problem

  type MD5 = Array[Byte]
  private val candidateCache = mutable.Set[MD5]()

  private var attemptsCount = 0

  private val log = Logger[PartialSchedulesGenerator]

  private def md5(str: String): MD5 = MessageDigest.getInstance("MD5").digest(str.getBytes)

  /** A lazy sequence of partial schedules. If the first one doesn't fit, go on. Ends when we can't backtrack any more. */
  def lazySeq(implicit random: Random, ctx: Context): Stream[Schedule] = {
    val slots = random.shuffle(problem.slots.toList)
    val topics = random.shuffle(problem.topics.toList)

    val initialState = backtrackAssignTopicsToSlots(Schedule.empty)(Queue(slots: _*), topics)
    Stream.iterate(initialState) {
      case Some(State(ps, sl, tl, tp)) => backtrackAssignTopicsToSlots(ps)(sl, tl, tp)
      case None => None
    }.collect {
      case Some(s) => s.partialSchedule
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
    * @return Some schedule that fits.
    */
  private def backtrackAssignTopicsToSlots(partialSchedule: Schedule)
    (slotsLeft: Queue[Slot], topicsLeft: List[Topic], topicsPassed: List[Topic] = Nil)
    (implicit ctx: Context): Option[State] = {

    if (ctx.debugMode) {
      val scheduleMd5 = md5(partialSchedule.toString)
      if (!candidateCache.add(scheduleMd5)) throw new IllegalStateException(partialSchedule.toFormattedString)
    }

    if (attemptsCount % 10000 == 0) log.debug(s"Tried $attemptsCount combinations")
    log.trace(s"Tried $attemptsCount combinations")
    attemptsCount += 1

    if (slotsLeft.isEmpty) {
      log.debug("Found a partial schedule")
      State(partialSchedule, slotsLeft, topicsLeft, topicsPassed).some

    } else if (topicsLeft.isEmpty) {
      /* No topic available for the current slot. If the current slot is not satisfied, we fail because there is no way to satisfy the current slot at this point. */
      if (maxPersonsOnSlot(partialSchedule, slotsLeft.head) < problem.personsCount) {
        log.trace("Fail because no topic available for current slot and it is not satisfied yet")
        None
      } else {
        /* go on without the current slot */
        log.trace("Go on without current clot because no topic available for it and it is already satisfied")
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
          log.trace("Go on without current clot because it has reached max parallelization and it is satisfied")
          backtrackAssignTopicsToSlots(partialSchedule)(slotsLeft.tail, topicsLeft ::: topicsPassed)
        }
      } else {
        /* We can try to add more topics to the current slot */
        val currentTopic = topicsLeft.head
        val nextTopics = topicsLeft.tail

        val record = Record(currentSlot, currentTopic, problem.mandatoryPersonsPerTopic(currentTopic)) //new record we want to try
        val candidate = partialSchedule + record // generate a new candidate with this record

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

object PartialSchedulesGenerator {


  /** Backtracking state
    * @param partialSchedule Partial schedule we are starting from
    * @param slotsLeft All slots on which we can still add some stuff, ordered by priority (we do a round-robin). Head is the current slot.
    * @param topicsLeft Topics we can try for the current slot.
    * @param topicsPassed Topics that won't work for the current slot, but may work for ulterior slots.
    */
  private case class State(partialSchedule: Schedule, slotsLeft: Queue[Slot], topicsLeft: List[Topic], topicsPassed: List[Topic] = Nil)

}