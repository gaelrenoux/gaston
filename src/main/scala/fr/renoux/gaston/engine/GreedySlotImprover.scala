package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.PlanningSpaceNavigator.Move
import fr.renoux.gaston.engine.assignment.{AssignmentImprover, ScheduleAssigner}
import fr.renoux.gaston.model._
import fr.renoux.gaston.util.Context
import fr.renoux.gaston.util.Context.chrono

import scala.util.Random


/** Improves a whole schedule by moving slots around. */
final class GreedySlotImprover(implicit problem: Problem, ctx: Context) extends Improver.Base[GreedySlotImprover.State] {

  import GreedySlotImprover.State

  private val log = Logger[GreedySlotImprover]

  private val navigator = new PlanningSpaceNavigator
  private val filler: ScheduleAssigner = new ScheduleAssigner
  private val personImprover: AssignmentImprover = new AssignmentImprover

  override protected def initialState(schedule: Schedule): State = State(schedule, Move.Nothing)

  /** Take an already improved schedule, and return the first better schedule it can find by swapping topics. */
  override protected def step(state: State)
    (implicit rand: Random): Option[(Schedule, GreedySlotImprover.State)] = chrono("GreedySlotImprover > improveOnce") {
    log.debug("New improver step")

    val neighbours: LazyList[(Schedule, Move)] = navigator.neighbours(state.schedule).distinctBy(_._1.planning)

    val improvedSchedules =
      for {
        (partial, move) <- neighbours
        if !move.reverts(state.previousMove)
        _ = log.debug(s"Trying that move: $move")
        unimproved <- filler.fill(partial)(rand)
        improved = personImprover.improve(unimproved)
        if improved.score > state.schedule.score
      } yield (improved, State(improved, move))

    improvedSchedules.headOption
    //TODO The schedule should come with some index, showing how many were examined. In the lazy list, only the improving schedules are kept (so "We have tried
    // 39 schedules" means that we have improved a schedule 39 times). In tests, it makes it harder to see how many schedules we actually tried, which would be
    // a better metric.
  }
}

object GreedySlotImprover {

  case class State(
      schedule: Schedule, previousMove: Move
  )

}
