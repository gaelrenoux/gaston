package fr.renoux.gaston.engine

import fr.renoux.gaston.model.Schedule
import fr.renoux.gaston.util.OptionImplicits._

import scala.util.Random

trait Improver {
  /** Lazy sequence of incrementing scored schedules, each accompanied with the number of schedules attempted. Non-empty,
    * ends when the schedule can't be improved anymore. */
  def improvements(schedule: Schedule, termination: Termination)(implicit rand: Random): LazyList[(Schedule, Long)]
}

object Improver {

  trait State {
    val schedule: Schedule
    val attemptsCount: Long
  }

  abstract class Base[S <: State] extends Improver {

    // TODO this part could actually be in the Engine itself, removing the need for the parent class of Improver.
    //  It seems more logical to me to have the improver work by single steps. State is passed through parameters anyway.

    /** Lazy sequence of incrementing scored schedules. Ends when the schedule can't be improved any more. Non-empty. */
    def improvements(
        initialSchedule: Schedule,
        termination: Termination
    )(implicit rand: Random): LazyList[(Schedule, Long)] = {
      val timeoutMs = termination.timeout.map(_.toEpochMilli)
      val init: Option[S] = Some(initialState(initialSchedule))
      lazy val unfolding: LazyList[(Schedule, Long)] = LazyList.unfold(init) {
        case None => None // stop the unfolding here
        case Some(state) => step(state).map { newState =>
          val schedule = newState.schedule
          val attemptsCount = newState.attemptsCount
          if (termination.score.exists(schedule.score >= _)) (schedule -> attemptsCount, None) // no state for next step, will stop unfolding
          else if (timeoutMs.exists(_ < System.currentTimeMillis())) (schedule -> attemptsCount, None) // no state for next step, will stop unfolding
          else (schedule -> attemptsCount, Some(newState))
        }
      }
      ((initialSchedule, 1L) #:: unfolding).distinct.optional(termination.intCount)(_.take(_))
    }

    protected def initialState(schedule: Schedule): S

    /** Returns the state to be transmitted to next step, and the best schedule found until now. If returning None, iteration stops. */
    protected def step(state: S)(implicit rand: Random): Option[S]
    // TODO Integrate Termination in here as well to check timeout more often
  }

}
