package fr.renoux.gaston.engine

import fr.renoux.gaston.model.Schedule

import scala.util.Random

/** A class responsible for taking a valid schedule and making it better by moving stuff around. */
trait ScheduleImprover {
  /** Generates a lazy sequence of generally-improving schedules (we might have some setbacks but as a whole, it should
    * go up and up), each accompanied with the number of schedules attempted. The generated sequence is non-empty as it
    * always contain at least the initial schedule passed as an argument.
    * ends when the schedule can't be improved anymore. */
  def improvements(initialSchedule: Schedule, termination: Termination)(implicit rand: Random): LazyList[(Schedule, Long)]
}

object ScheduleImprover {

  trait State {
    /** This is the latest schedule being generated by a step. */
    val schedule: Schedule

    /** This is how many schedules we've tried out (including the ones that we rejected because they weren't good enough). */
    val attemptsCount: Long

    /** The result of the latest step. */
    def result: (Schedule, Long) = (schedule, attemptsCount)
  }

  /** Typically, actual improvers work step by step, using a state representing where it's at in the improving process.
    * This is the parent class for improvers, containing a parameter representing the state they work on. */
  abstract class Base[S <: State] extends ScheduleImprover {

    /** Generates the sequence of improvements, given methods that provide the initial state, and how to produce a
      * schedule and transform the state at each step. */
    final def improvements(initialSchedule: Schedule, termination: Termination)(implicit rand: Random): LazyList[(Schedule, Long)] = {
      val init = initialState(initialSchedule)
      lazy val unfolding: LazyList[(Schedule, Long)] = LazyList.unfold(init) { state =>
        if (termination.checkTimeout()) None // reached timeout, we can stop
        else if (termination.checkCount(state.attemptsCount)) None // reached count, we can stop
        else if (termination.checkScore(state.schedule.score)) None // reached a good enough schedule on the last step, we can stop
        else step(state, termination).map { newState => (newState.result, newState) }
      }
      (initialSchedule, 1L) #:: unfolding
    }

    /** Generates the initial state from the very first schedule. */
    protected def initialState(schedule: Schedule): S

    /** Returns the state to be transmitted to next step. If it returns None, this is the end of the lazy sequence. */
    protected def step(state: S, termination: Termination)(implicit rand: Random): Option[S]
  }

}
