package fr.renoux.gaston.engine

import java.security.MessageDigest

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Person, Schedule, Slot, Topic}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.util.Random

/**
  * Uses backtracking to create a solution satisfying all constraints. Does not take preferences into account.
  */
class ConstrainedScheduleFactory(val problem: Problem, val debugMode: Boolean = false) {

  type MD5 = Array[Byte]
  private val candidateCache = mutable.Set[MD5]()

  private var count = 0

  private val log = Logger[ConstrainedScheduleFactory]


  /** Returns a Schedule satisfying all constraints, based on given random. Returns None if such a schedule cannot be
    *  constructed. */
  def makeSchedule(implicit random: Random): Option[Schedule] = {
    val slots = random.shuffle(problem.slots.toList)
    val topics = random.shuffle(problem.topics.toList)
    backtrackAssignTopicsToSlots(Schedule(problem.parallelization))(Queue(slots: _*), topics)(completePartialSchedule)
  }

  /** Commodity function, mainly for testing purposes */
  def makePartialSchedule(implicit random: Random): Option[Schedule] = {
    val slots = random.shuffle(problem.slots.toList)
    val topics = random.shuffle(problem.topics.toList)
    backtrackAssignTopicsToSlots(Schedule(problem.parallelization))(Queue(slots: _*), topics)(Some(_))
  }

  private def md5(str: String): MD5 = MessageDigest.getInstance("MD5").digest(str.getBytes)

  /**
    * Uses backtracking to construct a partial schedule with all topics assigned to slots, and mandatory people assigned to their topics. Then apply the post-treatment to have a complete schedule.
    *
    * @param partialSchedule Partial schedule we are starting from
    * @param slotsLeft       All slots on which we can still add some stuff, ordered by priority (we do a round-robin). Head is the current slot.
    * @param topicsLeft      Topics we can try for the current slot.
    * @param topicsPassed    Topics that won't work for the current slot, but may work for ulterior slots.
    * @param postTreatment   Post treatment to apply on the produced Schedule. If it returns none, the schedule was not acceptable so keep on backtracking.
    * @return Some schedule that fits.
    */
  private def backtrackAssignTopicsToSlots(partialSchedule: Schedule)
                                          (slotsLeft: Queue[Slot], topicsLeft: List[Topic], topicsPassed: List[Topic] = Nil)
                                          (postTreatment: Schedule => Option[Schedule]): Option[Schedule] = {

    if (debugMode) {
      val scheduleMd5 = md5(partialSchedule.toString)
      if (!candidateCache.add(scheduleMd5)) throw new IllegalStateException(partialSchedule.toFormattedString)
    }

    if (count % 1000 == 0) log.debug(s"Tried $count combinations")
    log.trace(s"Tried $count combinations")
    count += 1

    if (slotsLeft.isEmpty) {
      log.trace("All slots are satisfied and as much topics as possible have been assigned, apply postTreatment to see if solution is acceptable")
      postTreatment(partialSchedule)

    } else if (topicsLeft.isEmpty) {
      /* No topic available for the current slot. If the current slot is not satisfied, we fail because there is no way to satisfy the current slot at this point. */
      if (maxPersonsOnSlot(partialSchedule, slotsLeft.head) < problem.personsCount) {
        log.trace("Fail because no topic available for current slot and it is not satisfied yet")
        None
      } else {
        /* go on without the current slot */
        log.trace("Go on without current clot because no topic available for it and it is already satisfied")
        backtrackAssignTopicsToSlots(partialSchedule)(slotsLeft.tail, topicsLeft ::: topicsPassed)(postTreatment)
      }

    } else {
      val currentSlot = slotsLeft.head
      val currentTopic = topicsLeft.head
      val nextTopics = topicsLeft.tail

      val record = Schedule.Record(currentSlot, currentTopic, problem.mandatoryPersonsPerTopic(currentTopic)) //new record we want to try
      val candidate = partialSchedule.copy(records = partialSchedule.records + record) // generate a new candidate with this record

      val possibleSchedule =
        if (candidate.isSound && problem.isAcceptablePartial(candidate) && minPersonsOnSlot(candidate, currentSlot) <= problem.personsCount) {
          log.trace(s"Go on with acceptable candidate and next slot: $candidate")
          backtrackAssignTopicsToSlots(candidate)(slotsLeft.tail :+ currentSlot, nextTopics ::: topicsPassed, Nil)(postTreatment)
        } else None

      possibleSchedule .orElse {
        /* candidate is not acceptable or lead to a failure, try again with next topic */
        log.trace(s"Go on with new topic for current slot as the candidate is not OK")
        backtrackAssignTopicsToSlots(partialSchedule)(slotsLeft, nextTopics, currentTopic :: topicsPassed)(postTreatment)
      }
    }
  }

  private def minPersonsOnSlot(candidate: Schedule, slot: Slot) = candidate.topicsPerSlot.getOrElse(slot, Set()).toSeq.map(problem.minNumberPerTopic.getOrElse(_, 0)).sum

  private def maxPersonsOnSlot(candidate: Schedule, slot: Slot) = candidate.topicsPerSlot.getOrElse(slot, Set()).toSeq.map(problem.maxNumberPerTopic.getOrElse(_, problem.persons.size)).sum


  /** Starts with a partial schedule satisfying all constraints except number constraint, and generates a random
    * schedule respecting all constraints. */
  private def completePartialSchedule(partialSchedule: Schedule)(implicit random: Random): Option[Schedule] = {

    /** Iterate over Slots, stop in case there is an incompatibility on some Slot */
    @tailrec
    def completeForSlots(slots: List[Slot], scheduleOption: Option[Schedule]): Option[Schedule] = (slots, scheduleOption) match {
      case (Nil, _) => scheduleOption
      case (_, None) => None

      case (slot :: slotsTail, Some(schedule)) =>
        /* Handle current slot */

        val personsLeftSet = problem.personsPerSlot(slot) -- schedule.personsPerSlot(slot)
        val personsLeft = random.shuffle(personsLeftSet.toSeq)

        /*
        val topicsMinMaxCurrent = random.shuffle(
          schedule.topicsPerSlot(slot) map { t =>
            val min = problem.minNumberPerTopic.getOrElse(t, 0)
            val max = problem.maxNumberPerTopic.getOrElse(t, Int.MaxValue)
            val current = schedule.countPersonsPerTopic(t)
            (t, min, max, current)
          } toSeq
        )

        Dispatch.equallyWithMaxes(topicsMinMaxCurrent.map(_._3)) */

        val topicsWithValues = schedule.topicsPerSlot(slot). map { t =>
          val min = problem.minNumberPerTopic.getOrElse(t, 0)
          val max = problem.maxNumberPerTopic.getOrElse(t, Int.MaxValue)
          val current = schedule.countPersonsPerTopic(t)

          ((t, min - current), (t, max - math.max(min, current)))
        } .unzip

        val topicsNeedingMin = topicsWithValues._1 filter (_._2 > 0)
        val topicsOpenToMax = topicsWithValues._2 filter (_._2 > 0)

        val newSchedule = backtrackAssignPersonsToTopics(schedule)(topicsNeedingMin.toList, topicsOpenToMax.toList, personsLeft.toList, Nil, Nil)

        completeForSlots(slotsTail, newSchedule)
    }

    /* check wether it's possible to make it work first */
    partialSchedule.topicsPerSlot. find { case (slot, topics) =>
      val min = topics.toSeq.map(problem.minNumberPerTopic(_)).sum
      val max = topics.toSeq.map(problem.maxNumberPerTopic(_)).sum
      val pCount = problem.personsCountPerSlot(slot)
      pCount < min || pCount > max
    } match {
      case None => completeForSlots(problem.slots.toList, Some(partialSchedule))
      case Some((slot, _)) =>
        log.trace(s"Impossible to fill slot $slot")
        None
    }
  }


  /**
    * Assign persons to a list of topics.
    *
    * @param partialSchedule        Partial schedule from which we start
    * @param topicsNeedingMin       Topics to which we need to add people to reach the min number of persons
    * @param topicsOpenToMax        Topics to which we can add people up to the the max number of persons
    * @param personsLeft            Persons that need to be assigned to topics
    * @param personsSkipped         Persons that needs to be assigned to topics but which have been skipped for the head of the topic list
    * @param topicsOpenToMaxDelayed Topics that have there minimum value and which have been delayed
    * @return None if no schedule is possible, Some(schedule) if possible
    */
  private def backtrackAssignPersonsToTopics(partialSchedule: Schedule)
                                            (topicsNeedingMin: List[(Topic, Int)], topicsOpenToMax: List[(Topic, Int)], personsLeft: List[Person], personsSkipped: List[Person], topicsOpenToMaxDelayed: List[(Topic, Int)]): Option[Schedule] =
    (topicsNeedingMin, topicsOpenToMax, personsLeft) match {
      case (Nil, _, Nil) if personsSkipped.isEmpty =>
        log.debug("Finishing backtrackAssignPersonsToTopics because we have no more persons and all topics have their min numbers")
        Some(partialSchedule) // no more persons left and min numbers all reached !

      case (_, _, Nil) =>
        log.trace("Hit a dead end in backtrackAssignPersonsToTopics because we have no more persons and some topics don't have their min numbers")
        None //no more persons left and min numbers are not reached

      case (Nil, Nil, _) if topicsOpenToMaxDelayed.isEmpty =>
        log.trace("Hit a dead end in backtrackAssignPersonsToTopics because we have more persons and all topics have their max numbers")
        None //more persons left and max numbers are reached

      case (Nil, Nil, _) =>
        log.trace("No more topics open to max, go again with all delayed topics")
        backtrackAssignPersonsToTopics(partialSchedule)(topicsNeedingMin, topicsOpenToMaxDelayed, personsLeft ++ personsSkipped, Nil, Nil)

      case ((topic, _) :: _, _, person :: ptail) if problem.forbiddenPersonsPerTopic(topic)(person) =>
        log.trace("Current topic has not reached min number, but current person is forbidden on it: step to the next person")
        backtrackAssignPersonsToTopics(partialSchedule)(topicsNeedingMin, topicsOpenToMax, ptail, person :: personsSkipped, topicsOpenToMaxDelayed)

      case ((topic, count) :: ttail, _, person :: ptail) =>
        log.trace("Current topic has not reached min number");
        {
          /* Add current person and try to go on, with the same topic is we need more persons, or on the next topic */
          val newSchedule = partialSchedule.addPersonToTopic(person, topic)
          if (count == 1) backtrackAssignPersonsToTopics(newSchedule)(ttail, topicsOpenToMax, ptail ++ personsSkipped, Nil, topicsOpenToMaxDelayed)
          else backtrackAssignPersonsToTopics(newSchedule)((topic, count - 1) :: ttail, topicsOpenToMax, ptail, personsSkipped, topicsOpenToMaxDelayed)
        } orElse {
          /* If going on did not work, adding current person won't work so go to the next one */
          log.trace("Backtracking in backtrackAssignPersonsToTopics")
          backtrackAssignPersonsToTopics(partialSchedule)(topicsNeedingMin, topicsOpenToMax, ptail, person :: personsSkipped, topicsOpenToMaxDelayed)
        }

      case (Nil, (topic, _) :: _, person :: ptail) if problem.forbiddenPersonsPerTopic(topic)(person) =>
        log.trace("Current topic has not reached max number, but current person is forbidden on it: step to the next person")
        backtrackAssignPersonsToTopics(partialSchedule)(Nil, topicsOpenToMax, ptail, person :: personsSkipped, topicsOpenToMaxDelayed)

      case (Nil, (topic, count) :: ttail, person :: ptail) =>
        log.trace("Current topic has not reached max number");
        {
          /* Add current person and try to go on, with the next topic (current topic goes at the end if we can have more persons) */
          val newSchedule = partialSchedule.addPersonToTopic(person, topic)
          if (count == 1) backtrackAssignPersonsToTopics(newSchedule)(Nil, ttail, ptail ++ personsSkipped, Nil, topicsOpenToMaxDelayed)
          else backtrackAssignPersonsToTopics(newSchedule)(Nil, ttail, ptail ++ personsSkipped, Nil, (topic, count - 1) :: topicsOpenToMaxDelayed)
        } orElse {
          /* If going on did not work, adding current person won't work so go to the next one */
          log.trace("Backtracking in backtrackAssignPersonsToTopics")
          backtrackAssignPersonsToTopics(partialSchedule)(topicsNeedingMin, topicsOpenToMax, ptail, person :: personsSkipped, topicsOpenToMaxDelayed)
        }
    }

}

object ConstrainedScheduleFactory {

  /** Counting people on a certain topic */
  private case class PersonsCount(
                                   existing: Int, //how many persons are already present
                                   needed: Int, //how many persons are needed to reach the min
                                   optional: Int //how many persons can we add after the min is reached (or with existing number if already higher than min)
                                 )


}