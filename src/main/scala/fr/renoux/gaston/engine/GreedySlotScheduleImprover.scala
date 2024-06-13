package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.PlanningSpaceNavigator.Move
import fr.renoux.gaston.engine.assignment.{AssignmentImprover, RandomAssigner}
import fr.renoux.gaston.model._
import fr.renoux.gaston.util.Context
import fr.renoux.gaston.util.Context.chrono

import scala.util.Random


/** Improves a whole schedule by moving slots around. */
final class GreedySlotScheduleImprover(implicit problem: Problem, ctx: Context) extends ScheduleImprover.Base[GreedySlotScheduleImprover.State] {

  import GreedySlotScheduleImprover.State

  private val log = Logger[GreedySlotScheduleImprover]

  private val navigator = new PlanningSpaceNavigator
  private val randomAssigner: RandomAssigner = new RandomAssigner
  private val assignmentImprover: AssignmentImprover = new AssignmentImprover

  /** Initial attemptsCount is 1 because there's already the initial schedule */
  override protected def initialState(schedule: Schedule): State = State(schedule, Move.Nothing, 1)

  /** Take an already improved schedule, and return the first better schedule it can find by swapping topics. */
  override protected def step(state: State, termination: Termination)
    (implicit rand: Random): Option[GreedySlotScheduleImprover.State] = chrono("GreedySlotImprover > improveOnce") {
    log.debug("New improver step")

    val neighbours: LazyList[((Schedule, Move), Int)] = navigator.neighbours(state.schedule)
      .distinctBy(_._1.planning) // TODO Check how useful that is, I'm not sold
      .filterNot(_._2.reverts(state.previousMove))
      .zipWithIndex
      .takeWhile { _ => !termination.checkTimeout() } // stop when we reach timeout
      .takeWhile { case (_, index) => !termination.checkCount(index + state.attemptsCount) } // stop when we reach the max number of schedules to examine
    // Not checking the score: improving schedules are always reported up, where they can be checked, so no need to do it here

    val improvedSchedules =
      for {
        ((partial, move), index) <- neighbours
        _ = log.debug(s"Trying that move: $move")
        unimproved <- randomAssigner.fill(partial)(rand)
        improved = assignmentImprover.improve(unimproved)
        if improved.score > state.schedule.score
        _ = if (!improved.isSolution) {
          val message = s"A bad schedule was generated !\n${improved.toFormattedString}\n${improved.errors.mkString("\n")}\n\nLast move: $move"
          throw new IllegalStateException(message)
        }
      } yield State(improved, move, state.attemptsCount + index + 1)

    improvedSchedules.headOption
  }
}

object GreedySlotScheduleImprover {

  final case class State(
      schedule: Schedule, previousMove: Move, attemptsCount: Long
  ) extends ScheduleImprover.State

}
