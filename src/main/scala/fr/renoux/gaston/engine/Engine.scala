package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.RandomScheduleGenerator.BacktrackingFailures
import fr.renoux.gaston.model._
import fr.renoux.gaston.util.Context

import scala.util.Random


/** Main class for the engine. Its main method, `lazySeq`, produces a lazy list of schedules until it reaches somewhere
  * it cannot generate any more schedules. There is no guarantee about the time it may takes to reach the end, and it
  * might turn out being infinite. It is up to the caller to responsibly iterate on the lazy list.
  *
  * The Engine is immutable and thread-safe. You can use the same instance in multiple parallel threads to produce
  * different lazy sequences of schedule.
  */
abstract class Engine(triggerOnBacktrackingFailure: BacktrackingFailures => Unit = _ => ())(implicit problem: Problem, ctx: Context) {

  private val log = Logger[Engine]

  private lazy val startingScheduleGenerator = new RandomScheduleGenerator(triggerOnBacktrackingFailure)

  type State <: Engine.State

  /** Generates a lazy sequence of generally-improving schedules: we might have some setbacks but as a whole, it should
    * go up and up. Each schedule is accompanied with the number of schedules attempted since the beginning of the lazy
    * sueqence.
    *
    * The generated sequence is non-empty as it always contain at least the initial schedule passed as an argument.
    */
  final def lazySeq(chainSeed: Long, termination: Termination): LazyList[(Schedule, Long)] = {
    implicit val rand: Random = new Random(chainSeed)

    val initialSchedule: Schedule = if (problem.unassignedTopics.isEmpty) startingScheduleGenerator.create else Schedule.startingUnassignedOrForced(chainSeed)
    if (!initialSchedule.isSolution) {
      val message = s"A bad schedule was generated at startup !\n ${initialSchedule.toFormattedString}\n${initialSchedule.errors.mkString("\n")}"
      throw new IllegalStateException(message)
    } else if (initialSchedule.score.isNegativeInfinity) {
      val message = s"A bad schedule was generated at startup !\n ${initialSchedule.toFormattedString}\n(Score ${initialSchedule.score})"
      throw new IllegalStateException(message)
    } else log.debug(s"New initial schedule generated: ${initialSchedule.toFormattedString}")

    val init = initialState(initialSchedule)

    lazy val unfolding: LazyList[(Schedule, Long)] = LazyList.unfold(init) { state =>
      if (termination.checkTimeout()) None // reached timeout, we can stop
      else if (termination.checkCount(state.attemptsCount)) None // reached count, we can stop
      else if (termination.checkScore(state.schedule.score)) None // reached a good enough schedule on the last step, we can stop
      else step(state, termination).map { newState => (newState.result, newState) }
    }

    (init.result #:: unfolding).map { case (schedule, attempts) =>
      if (schedule.isSolution) (schedule, attempts) else {
        val message = s"A bad schedule was generated !\n ${schedule.toFormattedString}\n${schedule.errors.mkString("\n")}"
        throw new IllegalStateException(message)
      }
    }
  }

  /** Generates the initial state of the Engine from the very first schedule. */
  protected def initialState(schedule: Schedule): State

  /** Given a state of the engine, returns the next state generated. That state contains at least the next schedule to
    * return and the number of schedules tried since the start. If this method returns None, this is the end of the lazy
    * sequence. */
  protected def step(state: State, termination: Termination)(implicit rand: Random): Option[State]

}

object Engine {
  trait State {
    /** This is the latest schedule being generated by a step. */
    val schedule: Schedule

    /** This is how many schedules we've tried out since the beginning of the chain, including the ones that we rejected
      * because they weren't good enough. */
    val attemptsCount: Long

    /** The result of the latest step. */
    def result: (Schedule, Long) = (schedule, attemptsCount)
  }
}
