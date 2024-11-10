package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.PlanningSpaceNavigator.Move
import fr.renoux.gaston.engine.RandomScheduleGenerator.BacktrackingFailures
import fr.renoux.gaston.engine.assignment.{AssignmentImprover, RandomAssigner}
import fr.renoux.gaston.model.{Problem, Schedule}
import fr.renoux.gaston.util.Context
import fr.renoux.gaston.util.Context.chrono

import scala.math.Ordering.Implicits.infixOrderingOps
import scala.util.Random

/** A specific implementation of the Engine. At each step, we find a better neighbour and move there. The lazy-seq
 * produced has an ever-increasing score, and stops when there is no better neighbour (that is, at a local optimum).
 */
final class GreedyEngine(triggerOnBacktrackingFailure: BacktrackingFailures => Unit = _ => ())(using problem: Problem, ctx: Context)
  extends Engine(triggerOnBacktrackingFailure) {

  type State = GreedyEngine.State

  private val log = Logger[GreedyEngine]

  private val navigator = new PlanningSpaceNavigator
  private val randomAssigner: RandomAssigner = new RandomAssigner
  private val assignmentImprover: AssignmentImprover = new AssignmentImprover

  /** Initial attemptsCount is 1 because there's already the initial schedule */
  override protected def initialState(schedule: Schedule): State = GreedyEngine.State(schedule, Move.Nothing, 1)

  /** Take an already improved schedule, and return the first better schedule it can find by swapping topics. */
  override protected def step(state: State, termination: Termination)(using rand: Random): Option[State] = chrono("GreedyEngine > step") {
    log.debug("New engine step")

    val neighbours: LazyList[((Schedule, Move), Int)] = navigator.neighbours(state.schedule)
      .distinctBy(_._1.planning) // TODO Check how useful that is, I'm not sold
      .filterNot(_._2.reverts(state.previousMove)) // TODO Might also be useless, especially with the distinctBy just before
      .zipWithIndex
      .takeWhile { _ => !termination.checkTimeout() } // stop when we reach timeout
      .takeWhile { case (_, index) => !termination.checkCount(index + state.attemptsCount) } // stop when we reach the max number of schedules to examine
    // Not checking the score: improving schedules are always reported up, where they can be checked, so no need to do it here

    val improvedSchedules =
      for {
        ((unfilled, move), index) <- neighbours
        _ = log.debug(s"Trying that move: $move")
        unimproved <- randomAssigner.fill(unfilled)(using rand)
        improved = assignmentImprover.improve(unimproved)
        if improved.score > state.schedule.score
        _ = if (!improved.isSolution) {
          val message = s"A bad schedule was generated !\n${improved.toFormattedString}\n${improved.errors.mkString("\n")}\n\nLast move: $move"
          throw new IllegalStateException(message)
        }
      } yield GreedyEngine.State(improved, move, state.attemptsCount + index + 1)

    // FIXME All the schedules we've tried are not counted if find no acceptable schedule !

    improvedSchedules.headOption
  }
}

object GreedyEngine {

  /** In addition to the latest schedule and attempts-count, the state contains the latest move. It's used to avoid
   * trying again the previous schedule, but mostly it's useful as a debug tool. */
  final case class State(
      schedule: Schedule,
      previousMove: Move,
      attemptsCount: Long
  ) extends Engine.State

}
