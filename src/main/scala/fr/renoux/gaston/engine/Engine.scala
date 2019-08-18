package fr.renoux.gaston.engine

import fr.renoux.gaston.engine.PlanningSpaceNavigator.Move
import fr.renoux.gaston.engine.ScheduleGenerator.BacktrackingFailures
import fr.renoux.gaston.model._
import fr.renoux.gaston.util.Context

import scala.util.Random


class Engine(
    stopAtScore: Double = Double.MaxValue,
    maxImprovementRounds: Int = 1000,
    backtrackInitialSchedule: Boolean = false,
    triggerOnBacktrackingFailure: BacktrackingFailures => Unit = _ => ()
)(implicit problem: Problem, ctx: Context) {

  private val generator = new ScheduleGenerator(triggerOnBacktrackingFailure)
  private lazy val improver = new GreedySlotImprover(stopAtScore, maxImprovementRounds)

  lazy val startingSchedule: Schedule = Schedule.everyoneUnassigned

  /** Lazy sequence of incrementing scored schedules. Ends when the schedule can't be improved any more. Non-empty. */
  def lazySeq(seed: Long): LazyList[Schedule] = {
    implicit val rand: Random = new Random(seed)

    val initial: Option[(Schedule, Move)] =
      if (backtrackInitialSchedule) Some((generator.createOne, Move.Nothing))
      else Some((startingSchedule, Move.Nothing))

    LazyList.iterate(initial) {
      case None => None
      case Some((ss, move)) => improver.improveOnce(ss, move)
    }.takeWhile(_.isDefined).map(_.get._1).map { schedule =>
      if (schedule.isSolution) schedule else {
        val message = s"A bad schedule was generated !\n ${schedule.toFormattedString}"
        throw new IllegalStateException(message)
      }
    }
  }

  /** Produces a schedule and its score */
  def createOne(seed: Long): Schedule = {
    implicit val rand: Random = new Random(seed)

    val initial =
      if (backtrackInitialSchedule) generator.createOne
      else startingSchedule
    improver.improveToMax(initial)
  }

}
