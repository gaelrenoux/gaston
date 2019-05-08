package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.{Problem, Record, Schedule, Score, ScoredSchedule, Slot, Topic}
import fr.renoux.gaston.util.Tools

import scala.annotation.tailrec
import scala.util.Random

class Engine(
    problem: Problem,
    altImprover: Problem => ScheduleImprover = new GreedyScheduleImprover(_)
) {

  import Engine._

  private val log = Logger[Engine]

  val generator: PartialSchedulesGenerator = new PartialSchedulesGenerator(problem)
  val filler: PartialScheduleFiller = new PartialScheduleFiller(problem)
  val improver: ScheduleImprover = altImprover(problem)

  /** Produces a schedule and its score */
  def run(seed: Long)(implicit tools: Tools): ScoredSchedule = {
    implicit val _r: Random = new Random(seed)

    val Some(unimproved) = generateUnimproved
    val unimprovedScore = Scorer.score(unimproved)

    val improved = improve(unimproved, unimprovedScore)
    val improvedScore = Scorer.score(improved)

    recHeavyImprove(ScoredSchedule(improved, improvedScore))
  }

  /** For testing purposes */
  def generateUnimproved(implicit random: Random, ctx: Context, tools: Tools): Option[Schedule] =
    tools.chrono("ConstrainedScheduleFactory.makeSchedule") {
      generator.lazySeq.map(filler.fill).flatten.headOption
    }

  /** Improve the current schedule moving persons only. */
  private def improve(schedule: Schedule, score: Score)(implicit rand: Random, tools: Tools): Schedule =
    tools.chrono("ScheduleImprover.improve") {
      improver.improve(schedule, score)
    }

  /** Improve the schedule by trying swap after swap of topics. */
  @tailrec
  private def recHeavyImprove(schedule: ScoredSchedule, previousMove: Move = Move.Nothing, maxRound: Int = 1000)(implicit rand: Random, tools: Tools): ScoredSchedule =
    if (maxRound == 0) {
      log.warn(s"HeavyImprove could not do its best (score is ${schedule.score})")
      schedule
    } else heavyImprovement(schedule, previousMove) match {
      case None =>
        log.debug(s"[$maxRound] Best schedule I can get (score is ${schedule.score})")
        schedule //can't make it any better
      case Some((swappedSchedule, move)) =>
        log.debug(s"[$maxRound] Move: $move (new score is ${swappedSchedule.score}\n${swappedSchedule.schedule.toFormattedString}")
        recHeavyImprove(swappedSchedule, move, maxRound - 1)
    }

  private def shuffled[A](set: Set[A]): Seq[A] = Random.shuffle(set.toSeq)

  /** Take an already improved schedule, and return the first better schedule it can found by swapping topics. */
  private def heavyImprovement(scoredSchedule: ScoredSchedule, previousMove: Move)
    (implicit rand: Random, tools: Tools): Option[(ScoredSchedule, Move)] = {
    val ScoredSchedule(schedule, score) = scoredSchedule

    /* Swap topics between two scheduled topics */
    val allSwaps = for {
      (s1, s2) <- shuffled(problem.slotCouples).view
      t1 <- shuffled(schedule.topicsPerSlot(s1)).view
      t2 <- shuffled(schedule.topicsPerSlot(s2)).view
      move = Move.Swap(t1, t2)
      if !move.reverts(previousMove)

      /* Filter out impossible swaps because of mandatory persons */
      mandatoryPersonsTopic1 = problem.mandatoryPersonsPerTopic(t1)
      mandatoryPersonsSlot2 = schedule.mandatoryPersonsOnSlot(s2)
      if !mandatoryPersonsTopic1.exists(mandatoryPersonsSlot2) //check mandatories of T1 are not already blocked on S2
      mandatoryPersonsTopic2 = problem.mandatoryPersonsPerTopic(t2)
      mandatoryPersonsSlot1 = schedule.mandatoryPersonsOnSlot(s1)
      if !mandatoryPersonsTopic2.exists(mandatoryPersonsSlot1) //check mandatories of T2 are not already blocked on S1

      /* Generate the swap */
      partial = schedule.clearSlots(s1, s2).swapTopics(s1 -> t1, s2 -> t2)
      if partial.isPartialSolution
    } yield (partial, move)


    /* Swap topics between unscheduled and scheduled */
    val allExternalSwaps = for {
      newT <- shuffled(schedule.unscheduledTopics).view
      oldT <- shuffled(schedule.scheduledTopics).view
      move = Move.Swap(oldT, newT)
      if !move.reverts(previousMove)
      slot = schedule.topicToSlot(oldT)

      /* Filter out impossible swaps because of mandatory persons */
      mandatoryPersonsNewTopic = problem.mandatoryPersonsPerTopic(newT)
      mandatoryPersonsSlot = schedule.mandatoryPersonsOnSlot(slot)
      if !mandatoryPersonsNewTopic.exists(mandatoryPersonsSlot)

      /* Generate the swap */
      partial = schedule.clearSlots(slot).replaceTopic(oldT, newT)
      if partial.isPartialSolution
    } yield (partial, move)


    /* Add an unscheduled topic */
    val allAdds = for {
      slot <- shuffled(problem.slots).view
      topic <- shuffled(schedule.unscheduledTopics).view
      move = Move.Add(slot, topic)
      if !move.reverts(previousMove)

      /* Filter out impossible adds because of mandatory persons */
      mandatoryPersonsNewTopic = problem.mandatoryPersonsPerTopic(topic)
      mandatoryPersonsSlot = schedule.mandatoryPersonsOnSlot(slot)
      if !mandatoryPersonsNewTopic.exists(mandatoryPersonsSlot)

      /* Generate the swap */
      partial = schedule.clearSlots(slot).add(Record(slot, topic, mandatoryPersonsNewTopic))
      if partial.isPartialSolution

      /* Filter out impossible adds because of unreachable minimum */
      if partial.minPersonsOnSlot(slot) <= problem.personsCountPerSlot(slot)
    } yield (partial, move)


    /* Remove a scheduled topic */
    val allRemovals = for {
      slot <- shuffled(problem.slots).view
      topic <- shuffled(schedule.topicsPerSlot(slot)).view
      move = Move.Remove(slot, topic)
      if !move.reverts(previousMove)

      /* Generate the swap */
      partial = schedule.removeTopic(topic)
      if partial.isPartialSolution

      /* Filter out impossible adds because of maximum too low */
      if partial.maxPersonsOnSlot.getOrElse(slot, 0) >= problem.personsCountPerSlot(slot)
    } yield (partial, move)


    val improvedSchedules = for {
      (partial, move) <- allAdds ++ allSwaps ++ allExternalSwaps ++ allRemovals
      _ = log.debug(s"Trying that move: $move")
      unimproved <- filler.fill(partial)
      unimprovedScore = Scorer.score(unimproved)
      improved = improve(unimproved, unimprovedScore)
      improvedScore = Scorer.score(improved)
      if improvedScore > score
    } yield (ScoredSchedule(improved, improvedScore), move)

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