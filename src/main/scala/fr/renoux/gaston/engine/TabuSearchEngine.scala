package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.RandomScheduleGenerator.BacktrackingFailures
import fr.renoux.gaston.engine.assignment.{AssignmentImprover, RandomAssigner}
import fr.renoux.gaston.model.{Problem, Schedule}
import fr.renoux.gaston.util.Context

import scala.annotation.tailrec
import scala.util.Random

/** Use tabu search to improve a schedule. Not a real tabu search as we stop a step when we found a better schedule,
  * instead of always looking for the best.
  *
  * Doesn't seem to be very good.
  * TODO Rework or drop
  */
final class TabuSearchEngine(triggerOnBacktrackingFailure: BacktrackingFailures => Unit = _ => ())(implicit problem: Problem, ctx: Context)
  extends Engine(triggerOnBacktrackingFailure) {

  type State = TabuSearchEngine.State

  private val log = Logger[TabuSearchEngine]

  private val navigator = new PlanningSpaceNavigator
  private val randomAssigner: RandomAssigner = new RandomAssigner
  private val assignmentImprover: AssignmentImprover = new AssignmentImprover

  override protected def initialState(schedule: Schedule): State = TabuSearchEngine.State(
    best = schedule,
    current = schedule,
    tabu = Set(schedule.planning)
  )

  // TODO termination is unused, figure out if this class is useful before implementing it
  override protected def step(state: State, termination: Termination)(implicit rand: Random): Option[State] = {
    log.debug(s"Size of tabu is ${state.tabu.size}")

    val neighbours = navigator.neighbours(state.current).map(_._1).distinctBy(_.planning)

    val neighborhood: LazyList[Schedule] = for {
      n <- neighbours
      if !state.tabu.contains(n.planning)
      unimproved <- randomAssigner.fill(n)(rand)
      improved = assignmentImprover.improve(unimproved)
    } yield improved

    @tailrec
    def find(state: State, currentBest: Option[Schedule])(list: LazyList[Schedule]): Option[State] = list.headOption match {
      case None => currentBest.map(state.advance)
      case Some(s) if s.score > state.current.score => Some(state.advance(s))
      case Some(s) if currentBest.forall(_.score < s.score) => find(state.addTabu(s), Some(s))(list.tail)
      case Some(s) => find(state.addTabu(s), currentBest)(list.tail)
    }

    find(state, None)(neighborhood)
  }

}

object TabuSearchEngine {

  final case class State(
      best: Schedule,
      current: Schedule,
      tabu: Set[Schedule.Planning]
  ) extends Engine.State {

    override val schedule: Schedule = best

    override val attemptsCount: Long = 0 // TODO

    def advance(s: Schedule): State =
      if (s.score > best.score) copy(best = s, current = s, tabu = tabu + s.planning)
      else copy(current = s, tabu = tabu + s.planning)

    def addTabu(s: Schedule): State = copy(tabu = tabu + s.planning)
  }

}
