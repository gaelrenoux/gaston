package fr.renoux.gaston.engine

import fr.renoux.gaston.model.Schedule
import fr.renoux.gaston.util.OptionImplicits._

import scala.util.Random

trait Improver {
  /** Lazy sequence of incrementing scored schedules. Non-empty, may be finite or infinite. */
  def improvements(schedule: Schedule, params: OptimParams)(implicit rand: Random): LazyList[Schedule]
}

object Improver {

  abstract class Base[State] extends Improver {

    // TODO this part could actually be in the Engine itself, removing the need for the parent class of Improver.
    //  It seems more logical to me to have the improver work by single steps. State is passed through parameters anyway.
    /** Lazy sequence of incrementing scored schedules. Ends when the schedule can't be improved any more. Non-empty. */
    def improvements(
        schedule: Schedule,
        params: OptimParams
    )(implicit rand: Random): LazyList[Schedule] = {
      val timeoutMs = params.timeout.map(_.toEpochMilli)
      val init: Option[State] = Some(initialState(schedule))
      lazy val unfolding = LazyList.unfold(init) {
        case None => None // stop the unfolding here
        case Some(state) => step(state).map { case (schedule, newState) =>
          if (params.stopAtScore.exists(schedule.score.value >= _)) (schedule, None) // no state for next step, will stop unfolding
          else if (timeoutMs.exists(_ < System.currentTimeMillis())) (schedule, None) // no state for next step, will stop unfolding
          else (schedule, Some(newState))
        }
      }
      (schedule #:: unfolding).distinct.optional(params.maxImprovementRounds)(_.take(_))
    }

    protected def initialState(schedule: Schedule): State

    /** Returns the state to be transmitted to next step, and the best schedule found until now. If returning None, iteration stops. */
    protected def step(state: State)(implicit rand: Random): Option[(Schedule, State)]
  }

}
