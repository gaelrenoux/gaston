package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model._
import fr.renoux.gaston.util.Tools

import scala.annotation.tailrec
import scala.util.Random

class Engine(
    problem: Problem,
    stopAtScore: Double = Double.MaxValue,
    maxImprovementRounds: Int = 1000,
    backtrackInitialSchedule: Boolean = false
) {

  import Engine._

  private val log = Logger[Engine]

  private implicit val _p: Problem = problem

  private val initialScheduleGenerator  = new InitialScheduleGenerator(problem)
  private val filler: PartialScheduleFiller = new PartialScheduleFiller(problem)
  private val improver: ScheduleImprover = new ScheduleImprover(problem)

  lazy val startingSchedule: Schedule = Schedule.everyoneUnassigned



  /** Lazy sequence of incrementing scored schedules. Ends when the schedule can't be improved any more. Non-empty. */
  def lazySeq(seed: Long)(implicit tools: Tools): Stream[Schedule] = {
    implicit val _r: Random = new Random(seed)

    val initial: Option[(Schedule, Move)] =
      if (backtrackInitialSchedule) Some((initialScheduleGenerator.generate, Move.Nothing))
      else Some((startingSchedule, Move.Nothing))

    Stream.iterate(initial) {
      case None => None
      case Some((ss, move)) => heavyImprovement(ss, move)
    }.takeWhile(_.isDefined).map(_.get._1)
  }

  /** Produces a schedule and its score */
  def run(seed: Long)(implicit tools: Tools): Schedule = {
    implicit val _r: Random = new Random(seed)
    val initial =
      if (backtrackInitialSchedule) initialScheduleGenerator.generate
      else startingSchedule
    recHeavyImprove(initial)
  }

  /** Improve the current schedule moving persons only. */
  private def improve(scoredSchedule: Schedule)(implicit rand: Random, tools: Tools): Schedule =
    tools.chrono("ScheduleImprover.improve") {
      improver.improve(scoredSchedule)
    }

  /** Improve the schedule by trying swap after swap of topics. */
  @tailrec
  private def recHeavyImprove(schedule: Schedule, previousMove: Move = Move.Nothing, maxRound: Int = maxImprovementRounds)
    (implicit rand: Random, tools: Tools): Schedule =
    if (maxRound == 0) {
      log.warn(s"HeavyImprove could not do its best (score is ${schedule.score})")
      schedule
    } else if (schedule.score.value >= stopAtScore) {
      log.info(s"HeavyImprove stopped because expected score has been reached (score is ${schedule.score})")
      schedule
    } else heavyImprovement(schedule, previousMove) match {
      case None =>
        log.debug(s"[$maxRound] Best schedule I can get (score is ${schedule.score})")
        schedule //can't make it any better
      case Some((swappedSchedule, move)) =>
        log.debug(s"[$maxRound] Move: $move (new score is ${swappedSchedule.score}\n${swappedSchedule.toFormattedString}")
        recHeavyImprove(swappedSchedule, move, maxRound - 1)
    }

  private def shuffled[A](set: Set[A]): Seq[A] = Random.shuffle(set.toSeq)

  /** Take an already improved schedule, and return the first better schedule it can found by swapping topics. */
  private def heavyImprovement(schedule: Schedule, previousMove: Move)
    (implicit rand: Random, tools: Tools): Option[(Schedule, Move)] = {

    /* Add an unscheduled topic */
    lazy val allAdds = for {
      slot <- shuffled(problem.slots).view

      /* Filter out impossible adds because of incompatibility */
      slotSchedule = schedule.on(slot)
      topic <- shuffled(schedule.unscheduledTopics -- slotSchedule.currentIncompatibleTopics).view

      move = Move.Add(slot, topic)
      if !move.reverts(previousMove)

      /* Filter out impossible adds because mandatory persons are already taken */
      mandatoryPersonsSlot = slotSchedule.mandatory
      if !topic.mandatory.exists(mandatoryPersonsSlot)

      /* Generate the swap */
      partial = schedule.clearSlots(slot).add(Record(slot, topic, topic.mandatory))
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
      move = Move.Swap(t1, t2)
      if !move.reverts(previousMove)

      /* Filter out impossible swaps because of mandatory persons */
      if !t1.mandatory.exists(slotSchedule2.mandatory) //check mandatories of T1 are not already blocked on S2
      if !t2.mandatory.exists(slotSchedule1.mandatory) //check mandatories of T2 are not already blocked on S1

      /* Generate the swap */
      partial = schedule.clearSlots(s1, s2).swapTopics(s1 -> t1, s2 -> t2)
      if partial.isPartialSolution //TODO recheck everything, could maybe not check what has been verified already
    } yield (partial, move)


    /* Swap topics between unscheduled and scheduled */
    lazy val allExternalSwaps = for {
      newT <- shuffled(schedule.unscheduledTopics).view
      oldT <- shuffled(schedule.scheduledTopics).view
      move = Move.Swap(oldT, newT)
      if !move.reverts(previousMove)
      slot = schedule.topicToSlot(oldT)
      slotSchedule = schedule.on(slot)

      /* Filter out impossible swaps because of mandatory persons */
      if !newT.mandatory.exists(slotSchedule.mandatory)

      /* Generate the swap */
      partial = schedule.clearSlots(slot).replaceTopic(oldT, newT)
      if partial.isPartialSolution
    } yield (partial, move)


    /* Remove a scheduled topic */
    lazy val allRemovals = for {
      slot <- shuffled(problem.slots).view
      slotSchedule = schedule.on(slot)
      topic <- shuffled(slotSchedule.topics).view
      move = Move.Remove(slot, topic)
      if !move.reverts(previousMove)

      /* Generate the swap */
      partial = schedule.removeTopic(topic)
      if partial.isPartialSolution

      /* Filter out impossible adds because of maximum too low */
      if partial.maxPersonsOnSlot.getOrElse(slot, 0) >= problem.personsCountPerSlot(slot)
    } yield (partial, move)


    val improvedSchedules = for {
      (partial, move) <- allAdds.toStream #::: allSwaps.toStream #::: allExternalSwaps.toStream #::: allRemovals.toStream
      _ = log.debug(s"Trying that move: $move")
      unimproved <- filler.fill(partial)
      improved = improve(unimproved)
      if improved.score > schedule.score
    } yield (improved, move)

    improvedSchedules.headOption
  }
}

object Engine {

  /** Moves on the Schedule */
  trait Move {
    def reverts(m: Move): Boolean
  }

  object Move {

    case object Nothing extends Move {
      override def reverts(m: Move): Boolean = false
    }

    case class Swap(a: Topic, b: Topic) extends Move {
      override def reverts(m: Move): Boolean = m match {
        case Swap(a1, b1) if (a, b) == (a1, b1) || (a, b) == (b1, a1) => true
        case _ => false
      }
    }

    case class Add(s: Slot, t: Topic) extends Move {
      override def reverts(m: Move): Boolean = m match {
        case Remove(s1, t1) if s == s1 && t == t1 => true
        case _ => false
      }
    }

    case class Remove(s: Slot, t: Topic) extends Move {
      override def reverts(m: Move): Boolean = m match {
        case Add(s1, t1) if s == s1 && t == t1 => true
        case _ => false
      }
    }

  }

}