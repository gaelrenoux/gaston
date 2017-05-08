package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Person, Schedule, Slot, Topic}

import scala.collection.mutable
import scala.util.Random

/**
  * Created by gael on 07/05/17.
  */
object Engine {

  private val log = Logger(Engine.getClass)

  def getSolution(problem: Problem)(implicit random: Random) = {


  }

  def generateRandomSolution(problem: Problem, partialSchedule: Schedule)(implicit random: Random): Schedule = {

    val additions = for (slot <- problem.slots) yield {
      val personsLeftSet = problem.personsPerSlot(slot) -- partialSchedule.personsPerSlot(slot)
      var personsLeft = random.shuffle(personsLeftSet.toList)
      val topics = partialSchedule.topicsPerSlot(slot)
      val addedPersons = mutable.Map[Topic, mutable.Set[Person]]()
      topics.foreach(addedPersons(_) = mutable.Set[Person]())
      val topicPersonsCount = mutable.Map[Topic, Int]()
      topics.foreach(topicPersonsCount(_) = 0)

      /* Calculate the number of persons needed to reach the min */
      val neededAdditionalPersonsCount = topics map { t =>
        val min = problem.minNumberPerTopic.getOrElse(t, 0)
        val existing = partialSchedule.countPersonsPerTopic(t)
        topicPersonsCount(t) = existing
        t -> math.max(0, min - existing)
      } filter (_._2 > 0) toMap

      /** Reach the min for every topic */
      for ((t, count) <- neededAdditionalPersonsCount) {
        val ps = addedPersons(t)
        ps ++= personsLeft.take(count) //TODO check size
        topicPersonsCount(t) = topicPersonsCount(t) + count
        personsLeft = personsLeft.drop(count)
      }

      /** Put everyone left */
      var topicSeq = random.shuffle(topics.toSeq)
      while (personsLeft.nonEmpty) {
        topicSeq = topicSeq filter { t => topicPersonsCount(t) < problem.maxNumberPerTopic.getOrElse(t, Int.MaxValue) }
        for (t <- topicSeq) {
          if (personsLeft.nonEmpty) {
            val p = personsLeft.head
            addedPersons(t) += p
            topicPersonsCount(t) = topicPersonsCount(t) + 1
            personsLeft = personsLeft.tail
          }
        }
      }

      log.debug(s"Added persons on slot $slot: $addedPersons")
      slot -> addedPersons.mapValues(_.toSet).toSet
    }

    val addedTriplets = additions flatMap { case (s, tps) =>
      tps map { case (t, ps) => (s, t, ps) }
    }

    partialSchedule.merge(addedTriplets)
  }

  def initializeScheduleForConstraints(problem: Problem)(implicit random: Random): Option[Schedule] = {
    val slots = random.shuffle(problem.slots.toList)
    val topics = random.shuffle(problem.topics.toList)
    backtrackForConstraints(problem, Schedule())(slots, 0, topics)
  }

  private def backtrackForConstraints(problem: Problem, partialSchedule: Schedule)
                                     (slotsLeft: List[Slot], currentSlotCount: Int, topicsLeft: List[Topic], topicsPassed: List[Topic] = Nil): Option[Schedule] =

    if (topicsLeft.isEmpty && topicsPassed.isEmpty) Some(partialSchedule)
    else if (topicsLeft.isEmpty) None
    else {
      val currentSlot = slotsLeft.head
      val currentTopic = topicsLeft.head
      val newTopicsLeft = topicsLeft.tail

      val triplet = (currentSlot, currentTopic, problem.mandatoryPersonsPerTopic(currentTopic))
      val candidate = partialSchedule.copy(triplets = partialSchedule.triplets + triplet)
      val candidateAcceptable = problem.constraints forall { c => !c.isApplicableToPartialSolution || c.isRespected(candidate) }
      if (candidateAcceptable) {
        val newSlotCount = (currentSlotCount + 1) % problem.parallelization
        val newSlotsLeft = if (newSlotCount == 0) slotsLeft.tail else slotsLeft
        val rec = backtrackForConstraints(problem, candidate)(newSlotsLeft, newSlotCount, topicsPassed ::: newTopicsLeft)
        if (rec.isDefined) return rec
      } else {
        log.debug(s"Candidate was not acceptable: $candidate")
      }

      log.debug(s"Backtracking from: $candidate")

      /* candidate was not acceptable, or went into a dead end. Either we can try with next topic, or we're done */
      if (newTopicsLeft.isEmpty) None
      else backtrackForConstraints(problem, partialSchedule)(slotsLeft, currentSlotCount, newTopicsLeft, currentTopic :: topicsPassed)
    }


}
