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
      val initial: (Int, State) = (maxImprovementRounds, initialState(schedule))
      (schedule #:: LazyList.unfold[Schedule, (Int, State)](initial) {
        case (0, _) => None // max number of rounds reached
        case (n, state) => step(state).map { case (schedule, newState) =>
          if (schedule.score.value >= stopAtScore) (schedule, (0, newState))
          else (schedule, (n - 1, newState))
        }
      }).distinct
    }

    protected def initialState(schedule: Schedule): State

    protected def step(state: State)(implicit rand: Random): Option[(Schedule, State)]
  }

}