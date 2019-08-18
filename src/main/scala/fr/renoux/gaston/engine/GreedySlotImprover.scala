package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.PlanningSpaceNavigator.Move
import fr.renoux.gaston.engine.assignment.{AssignmentImprover, ScheduleAssigner}
import fr.renoux.gaston.model._
import fr.renoux.gaston.util.Context
import fr.renoux.gaston.util.Context.chrono

import scala.annotation.tailrec
import scala.util.Random


/** Improves a whole schedule by moving slots around. */
class GreedySlotImprover(
    stopAtScore: Double = Double.MaxValue,
    maxImprovementRounds: Int = 1000
)(implicit private val problem: Problem, ctx: Context) {

  private val log = Logger[GreedySlotImprover]

  private val navigator = new PlanningSpaceNavigator
  private val filler: ScheduleAssigner = new ScheduleAssigner
  private val personImprover: AssignmentImprover = new AssignmentImprover

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
        schedule // can't make it any better
      case Some((swappedSchedule, move)) =>
        log.debug(s"[$maxRound] Move: $move (new score is ${swappedSchedule.score}\n${swappedSchedule.toFormattedString}")
        improveToMax(swappedSchedule, move, maxRound - 1)
    }


  /** Take an already improved schedule, and return the first better schedule it can find by swapping topics. */
  def improveOnce(schedule: Schedule, previousMove: Move)
    (implicit rand: Random): Option[(Schedule, Move)] = chrono("SlotImprover > improveOnce") {

    val neighbours = navigator.neighbours(schedule)

    val improvedSchedules = chrono("SlotImprover > improveOnce > improvedSchedules") {
      for {
        (partial, move) <- neighbours
        if !move.reverts(previousMove)
        _ = log.debug(s"Trying that move: $move")
        unimproved <- filler.fill(partial)(rand)
        improved = personImprover.improve(unimproved)
        if improved.score > schedule.score
      } yield (improved, move)
    }

    chrono("SlotImprover > improveOnce > computation") {
      improvedSchedules.headOption
    }
  }

}
