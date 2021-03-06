package fr.renoux.gaston.engine

import fr.renoux.gaston.engine.ScheduleGenerator.BacktrackingFailures
import fr.renoux.gaston.model._
import fr.renoux.gaston.util.Context

import scala.util.Random


final class Engine(
    backtrackInitialSchedule: Boolean = false,
    triggerOnBacktrackingFailure: BacktrackingFailures => Unit = _ => ()
)(implicit problem: Problem, improver: Improver, ctx: Context) {

  private val generator = new ScheduleGenerator(triggerOnBacktrackingFailure)

  lazy val startingSchedule: Schedule = Schedule.everyoneUnassigned

  /** Lazy sequence of incrementing scored schedules. Ends when the schedule can't be improved any more. Non-empty. */
  def lazySeq(seed: Long, params: OptimParams): LazyList[Schedule] = {
    implicit val rand: Random = new Random(seed)

    val initial: Schedule = if (backtrackInitialSchedule) generator.createOne else startingSchedule

    improver.improvements(initial, params).map { schedule =>
      if (schedule.isSolution) schedule else {
        val message = s"A bad schedule was generated !\n ${schedule.toFormattedString}"
        throw new IllegalStateException(message)
      }
    }
  }

}
