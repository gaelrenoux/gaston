package fr.renoux.gaston.engine

import fr.renoux.gaston.engine.ScheduleGenerator.BacktrackingFailures
import fr.renoux.gaston.engine.SlotImprover.Move
import fr.renoux.gaston.model._

import scala.util.Random

class Engine(
    stopAtScore: Double = Double.MaxValue,
    maxImprovementRounds: Int = 1000,
    backtrackInitialSchedule: Boolean = false,
    triggerOnBacktrackingFailure: BacktrackingFailures => Unit = _ => ()
)(implicit problem: Problem, ctx: Context) {

  private val generator = new ScheduleGenerator(triggerOnBacktrackingFailure)
  private lazy val improver = new SlotImprover(stopAtScore, maxImprovementRounds)

  lazy val startingSchedule: Schedule = Schedule.everyoneUnassigned

  /** Lazy sequence of incrementing scored schedules. Ends when the schedule can't be improved any more. Non-empty. */
  def lazySeq(seed: Long): Stream[Schedule] = {
    implicit val rand: Random = new Random(seed)

    val initial: Option[(Schedule, Move)] =
      if (backtrackInitialSchedule) Some((generator.createOne, Move.Nothing))
      else Some((startingSchedule, Move.Nothing))

    Stream.iterate(initial) {
      case None => None
      case Some((ss, move)) => improver.improveOnce(ss, move)
    }.takeWhile(_.isDefined).map(_.get._1)
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
