package fr.renoux.gaston.engine

import java.time.Instant

import fr.renoux.gaston.engine.ScheduleGenerator.BacktrackingFailures
import fr.renoux.gaston.model._
import fr.renoux.gaston.util.Context

import scala.util.Random


class Engine(
    stopAtScore: Double = Double.MaxValue,
    maxImprovementRounds: Int = 1000,
    timeout: Instant = Instant.MAX,
    backtrackInitialSchedule: Boolean = false,
    triggerOnBacktrackingFailure: BacktrackingFailures => Unit = _ => ()
)(implicit problem: Problem, ctx: Context) {

  private val generator = new ScheduleGenerator(triggerOnBacktrackingFailure)
  private lazy val improver: Improver = new GreedySlotImprover(stopAtScore, maxImprovementRounds, timeout)

  lazy val startingSchedule: Schedule = Schedule.everyoneUnassigned

  /** Lazy sequence of incrementing scored schedules. Ends when the schedule can't be improved any more. Non-empty. */
  def lazySeq(seed: Long): LazyList[Schedule] = {
    implicit val rand: Random = new Random(seed)

    val initial: Schedule = if (backtrackInitialSchedule) generator.createOne else startingSchedule

    improver.improvements(initial).map { schedule =>
      if (schedule.isSolution) schedule else {
        val message = s"A bad schedule was generated !\n ${schedule.toFormattedString}"
        throw new IllegalStateException(message)
      }
    }
  }

}
