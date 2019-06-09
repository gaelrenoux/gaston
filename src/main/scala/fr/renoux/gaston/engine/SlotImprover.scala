package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model._

import scala.annotation.tailrec
import scala.util.Random

/** Improves a whole schedule by moving slots around. */
class SlotImprover(
    stopAtScore: Double = Double.MaxValue,
    maxImprovementRounds: Int = 1000
)(implicit private val problem: Problem, ctx: Context) {

  import SlotImprover._

  private val log = Logger[SlotImprover]

  private val filler: PartialScheduleFiller = new PartialScheduleFiller
  private val personImprover: PersonPlacementImprover = new PersonPlacementImprover

  /** Improve the schedule by trying swap after swap of topics. */
  @tailrec
  final def improveToMax(schedule: Schedule, previousMove: Move = Move.Nothing, maxRound: Int = maxImprovementRounds)
    (implicit rand: Random): Schedule =
    if (maxRound == 0) {
      log.warn(s"Improver could not do its best (score is ${schedule.score})")
      schedule
    } else if (schedule.score.value >= stopAtScore) {
      log.info(s"Improver stopped because expected score has been reached (score is ${schedule.score})")
      schedule
    } else improveOnce(schedule, previousMove) match {
      case None =>
        log.debug(s"[$maxRound] Best schedule I can get (score is ${schedule.score})")
        schedule //can't make it any better
      case Some((swappedSchedule, move)) =>
        log.debug(s"[$maxRound] Move: $move (new score is ${swappedSchedule.score}\n${swappedSchedule.toFormattedString}")
        improveToMax(swappedSchedule, move, maxRound - 1)
    }


  /** Take an already improved schedule, and return the first better schedule it can found by swapping topics. */
  def improveOnce(schedule: Schedule, previousMove: Move)
    (implicit rand: Random): Option[(Schedule, Move)] = {

    /* Add an unscheduled topic */
    lazy val allAdds = for {
      slot <- shuffled(problem.slots).view

      /* Filter out impossible adds because of incompatibility */
      slotSchedule = schedule.on(slot)
      topic <- shuffled(schedule.unscheduledTopics -- slotSchedule.currentIncompatibleTopics).view
      topicsToAdd = linkedTopics(topic)

      move = Move.Add(slot, topicsToAdd)
      if !move.reverts(previousMove)

      /* Filter out impossible adds because mandatory persons are already taken */
      mandatoryPersonsSlot = slotSchedule.mandatory
      if !topicsToAdd.flatMap(_.mandatory).exists(mandatoryPersonsSlot)

      /* Generate the swap */
      records = topicsToAdd.map { t => Record(slot, t, t.mandatory) }
      partial = schedule.clearSlots(slot).add(records)
      if partial.isPartialSolution

      /* Filter out impossible adds because of unreachable minimum */
      if partial.minPersonsOnSlot(slot) <= problem.personsCountPerSlot(slot)
    } yield (partial, move)

    /* Swap topics between two scheduled topics */
    lazy val allSwaps = for {
      (s1, s2) <- shuffled(problem.slotCouples).view
      slotSchedule1 = schedule.on(s1)
      slotSchedule2 = schedule.on(s2)

      t1 <- shuffled(slotSchedule1.topics -- slotSchedule2.hardIncompatibleTopics).view //can't take current topics since we're removing one
      t2 <- shuffled(slotSchedule2.topics -- slotSchedule1.hardIncompatibleTopics).view //can't take current topics since we're removing one
      topics1 = linkedTopics(t1)
      topics2 = linkedTopics(t2)
      move = Move.Swap(topics1, topics2)
      if !move.reverts(previousMove)

      /* Filter out impossible swaps because of mandatory persons */
      if !topics1.flatMap(_.mandatory).exists(slotSchedule2.mandatory) //check mandatories of T1 are not already blocked on S2
      if !topics2.flatMap(_.mandatory).exists(slotSchedule1.mandatory) //check mandatories of T2 are not already blocked on S1

      /* Generate the swap */
      partial = schedule.clearSlots(s1, s2).swapTopics(s1 -> topics1, s2 -> topics2)
      if partial.isPartialSolution //TODO this rechecks everything, we shouldn't check what has been checked individually before
    } yield (partial, move)


    /* Swap topics between unscheduled and scheduled */
    lazy val allExternalSwaps = for {
      newT <- shuffled(schedule.unscheduledTopics).view
      oldT <- shuffled(schedule.scheduledTopics).view
      newTs = linkedTopics(newT)
      oldTs = linkedTopics(oldT)
      move = Move.Swap(oldTs, newTs)
      if !move.reverts(previousMove)
      slot = schedule.topicToSlot(oldT)
      slotSchedule = schedule.on(slot)

      /* Filter out impossible swaps because of mandatory persons */
      if !newTs.flatMap(_.mandatory).exists(slotSchedule.mandatory)

      /* Generate the swap */
      partial = schedule.clearSlots(slot).replaceTopics(slot, oldTs, newTs)
      if partial.isPartialSolution
    } yield (partial, move)


    /* Remove a scheduled topic */
    lazy val allRemovals = for {
      slot <- shuffled(problem.slots).view
      slotSchedule = schedule.on(slot)
      topic <- shuffled(slotSchedule.topics).view
      topicsToRemove = linkedTopics(topic)

      move = Move.Remove(slot, topicsToRemove)
      if !move.reverts(previousMove)

      /* Generate the swap */
      partial = schedule.removeTopics(topicsToRemove)
      if partial.isPartialSolution

      /* Filter out impossible adds because of maximum too low */
      if partial.maxPersonsOnSlot.getOrElse(slot, 0) >= problem.personsCountPerSlot(slot)
    } yield (partial, move)


    val improvedSchedules = for {
      (partial, move) <- allAdds.toStream #::: allSwaps.toStream #::: allExternalSwaps.toStream #::: allRemovals.toStream
      _ = log.debug(s"Trying that move: $move")
      unimproved <- filler.fill(partial)(rand)
      improved = improve(unimproved)
      if improved.score > schedule.score
    } yield (improved, move)

    improvedSchedules.headOption
  }

  /** Improve the current schedule moving persons only. */
  private def improve(scoredSchedule: Schedule)(implicit rand: Random): Schedule =
    ctx.tools.chrono("ScheduleImprover.improve") {
      personImprover.improve(scoredSchedule)
    }

  private def shuffled[A](set: Set[A]): Seq[A] = Random.shuffle(set.toSeq)

  private def linkedTopics(topic: Topic): Set[Topic] = problem.simultaneousTopicPerTopic(topic) + topic

}

object SlotImprover {


  /** Moves on the Schedule */
  trait Move {
    def reverts(m: Move): Boolean
  }

  object Move {

    case object Nothing extends Move {
      override def reverts(m: Move): Boolean = false
    }

    case class Swap(a: Set[Topic], b: Set[Topic]) extends Move {
      override def reverts(m: Move): Boolean = m match {
        case Swap(a1, b1) if (a, b) == (a1, b1) || (a, b) == (b1, a1) => true
        case _ => false
      }
    }

    case class Add(s: Slot, t: Set[Topic]) extends Move {
      override def reverts(m: Move): Boolean = m match {
        case Remove(s1, t1) if s == s1 && t == t1 => true
        case _ => false
      }
    }

    case class Remove(s: Slot, t: Set[Topic]) extends Move {
      override def reverts(m: Move): Boolean = m match {
        case Add(s1, t1) if s == s1 && t == t1 => true
        case _ => false
      }
    }

  }

}