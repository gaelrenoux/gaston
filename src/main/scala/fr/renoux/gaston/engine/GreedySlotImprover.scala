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

  /** Initial attemptsCount is 1 because there's already the initial schedule */
  override protected def initialState(schedule: Schedule): State = State(schedule, Move.Nothing, 1)

  /** Take an already improved schedule, and return the first better schedule it can find by swapping topics. */
  override protected def step(state: State)
    (implicit rand: Random): Option[GreedySlotImprover.State] = chrono("GreedySlotImprover > improveOnce") {
    log.debug("New improver step")

    val neighbours: LazyList[(Schedule, Move)] = navigator.neighbours(state.schedule).distinctBy(_._1.planning)
    var attemptsCount = 0

    val improvedSchedules =
      for {
        (partial, move) <- neighbours
        if !move.reverts(state.previousMove)
        _ = log.debug(s"Trying that move: $move")
        unimproved <- filler.fill(partial)(rand)
        improved = personImprover.improve(unimproved)
        _ = attemptsCount += 1 // TODO Ugly, we could also do a zipWithIndex but let's check performance
        if improved.score > state.schedule.score
        _ = if (!improved.isSolution) {
          val message = s"A bad schedule was generated !\n${improved.toFormattedString}\n${improved.errors.mkString("\n")}\n\nLast move: $move"
          throw new IllegalStateException(message)
        }
      } yield State(improved, move, state.attemptsCount + attemptsCount)

    improvedSchedules.headOption
  }
}

object GreedySlotImprover {

  /** @param attemptsCount How many schedule attempts it took to reach the current state */
  final case class State(
      schedule: Schedule, previousMove: Move, attemptsCount: Long
  ) extends Improver.State

}
