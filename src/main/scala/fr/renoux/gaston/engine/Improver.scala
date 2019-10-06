package fr.renoux.gaston.engine

import java.time.Instant

import fr.renoux.gaston.model.Schedule

import scala.util.Random

trait Improver {
  /** Lazy sequence of incrementing scored schedules. Non-empty, may be finite or infinite. */
  def improvements(schedule: Schedule)(implicit rand: Random): LazyList[Schedule]
}

object Improver {

  abstract class Base[State] extends Improver {
    val stopAtScore: Double
    val maxImprovementRounds: Int
    val timeout: Instant

    private val timeoutMs = timeout.toEpochMilli

    /** Lazy sequence of incrementing scored schedules. Ends when the schedule can't be improved any more. Non-empty. */
    def improvements(schedule: Schedule)(implicit rand: Random): LazyList[Schedule] = {
      val init: Option[State] = Some(initialState(schedule))
      (schedule #:: LazyList.unfold(init) {
        case None => None // stop the unfolding here
        case Some(state) => step(state).map { case (schedule, newState) =>
          if (schedule.score.value >= stopAtScore) (schedule, None) // no state for next step, will stop unfolding
          else if (System.currentTimeMillis() > timeoutMs) (schedule, None) // no state for next step, will stop unfolding
          else (schedule, Some(newState))
        }
      }).take(maxImprovementRounds).distinct
    }

    protected def initialState(schedule: Schedule): State

    /** Returns the state to be transmitted to next step, and the best schedule found until now. If returning None, iteration stops. */
    protected def step(state: State)(implicit rand: Random): Option[(Schedule, State)]
  }

}