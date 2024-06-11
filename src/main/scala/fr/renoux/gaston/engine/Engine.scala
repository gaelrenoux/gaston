package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.ScheduleGenerator.BacktrackingFailures
import fr.renoux.gaston.model._
import fr.renoux.gaston.util.Context

import scala.util.Random


/** Main class for the engine. Its main method, `lazySeq`, produces a lazy list of improving schedules until it reaches
  * a local maximum, at which point it terminate. There is no guarantee about the time it may takes to reach that
  * maximum, and also mathematically there must be at least a global maximum, it might be near-inifinite in practice. It
  * is up to the caller to responsibly iterate on the lazy list.
  *
  * The Engine is immutable and thread-safe. You can use the same instance in multiple parallel threads to produce
  * different lazy sequences of schedule.
  */
final class Engine(
    triggerOnBacktrackingFailure: BacktrackingFailures => Unit = _ => ()
)(implicit problem: Problem, improver: Improver, ctx: Context) {

  private val log = Logger[Engine]

  private val scheduleBacktrackingGenerator = new ScheduleGenerator(triggerOnBacktrackingFailure)

  /** Lazy sequence of incrementing scored schedules, with the number of schedules attempted. Ends when the schedule can't be improved any more. Non-empty. */
  def lazySeq(chainSeed: Long, termination: Termination): LazyList[(Schedule, Long)] = {
    implicit val rand: Random = new Random(chainSeed)

    val initial: Schedule = if (problem.unassignedTopics.isEmpty) scheduleBacktrackingGenerator.createOne else Schedule.startingUnassignedOrForced(chainSeed)
    if (!initial.isSolution) {
      val message = s"A bad schedule was generated at startup !\n ${initial.toFormattedString}\n${initial.errors.mkString("\n")}"
      throw new IllegalStateException(message)
    } else if (initial.score.isNegativeInfinity) {
      val message = s"A bad schedule was generated at startup !\n ${initial.toFormattedString}\n(Score ${initial.score})"
      throw new IllegalStateException(message)
    } else log.debug(s"New initial schedule generated: ${initial.toFormattedString}")

    improver.improvements(initial, termination).map { case (schedule, attempts) =>
      if (schedule.isSolution) (schedule, attempts) else {
        val message = s"A bad schedule was generated !\n ${schedule.toFormattedString}\n${schedule.errors.mkString("\n")}"
        throw new IllegalStateException(message)
      }
    }
  }

}
