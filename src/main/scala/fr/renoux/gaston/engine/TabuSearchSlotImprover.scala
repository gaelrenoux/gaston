package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.assignment.{AssignmentImprover, ScheduleAssigner}
import fr.renoux.gaston.model._
import fr.renoux.gaston.util.Context

import scala.annotation.tailrec
import scala.util.Random


/** Use tabu search to improve a schedule. Not a real tabu search as we stop a step when we found a better schedule,
  * instead of always looking for the best.
  *
  * Doesn't seem to be very good.
  */
final class TabuSearchSlotImprover(implicit problem: Problem, ctx: Context) extends Improver.Base[TabuSearchSlotImprover.State] {

  import TabuSearchSlotImprover.State

  private val log = Logger[TabuSearchSlotImprover]

  private val navigator = new PlanningSpaceNavigator
  private val filler: ScheduleAssigner = new ScheduleAssigner
  private val personImprover: AssignmentImprover = new AssignmentImprover

  override protected def initialState(schedule: Schedule): State = State(
    best = schedule,
    current = schedule,
    tabu = Set(schedule.planning)
  )

  override protected def step(state: State)(implicit rand: Random): Option[(Schedule, State)] = {
    log.debug(s"Size of tabu is ${state.tabu.size}")

    val neighbours = navigator.neighbours(state.current).map(_._1).distinctBy(_.planning)

    val neighborhood: LazyList[Schedule] = for {
      n <- neighbours
      if !state.tabu.contains(n.planning)
      unimproved <- filler.fill(n)(rand)
      improved = personImprover.improve(unimproved)
    } yield improved

    @tailrec
    def find(state: State, currentBest: Option[Schedule])(list: LazyList[Schedule]): Option[State] = list.headOption match {
      case None => currentBest.map(state.advance)
      case Some(s) if s.score > state.current.score => Some(state.advance(s))
      case Some(s) if currentBest.forall(_.score < s.score) => find(state.addTabu(s), Some(s))(list.tail)
      case Some(s) => find(state.addTabu(s), currentBest)(list.tail)
    }

    find(state, None)(neighborhood).map { s => (s.best, s) }
  }

}

object TabuSearchSlotImprover {

  case class State(
      best: Schedule,
      current: Schedule,
      tabu: Set[Schedule.Planning]
  ) {
    def advance(s: Schedule): State =
      if (s.score > best.score) copy(best = s, current = s, tabu = tabu + s.planning)
      else copy(current = s, tabu = tabu + s.planning)

    def addTabu(s: Schedule): State = copy(tabu = tabu + s.planning)
  }

}
