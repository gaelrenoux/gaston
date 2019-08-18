package fr.renoux.gaston.engine

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

    /** Lazy sequence of incrementing scored schedules. Ends when the schedule can't be improved any more. Non-empty. */
    def improvements(schedule: Schedule)(implicit rand: Random): LazyList[Schedule] = {
      (schedule #:: LazyList.unfold[Schedule, (Option[State])](Some(initialState(schedule))) {
        case None => None // no state after last schedule
        case Some(state) => step(state).map { case (schedule, newState) =>
          if (schedule.score.value >= stopAtScore) (schedule, None) // no state for next round
          else (schedule, Some(newState))
        }
      }).take(maxImprovementRounds).distinct
    }

    protected def initialState(schedule: Schedule): State

    protected def step(state: State)(implicit rand: Random): Option[(Schedule, State)]
  }

}