package fr.renoux.gaston.engine

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.assignment.{AssignmentImprover, ScheduleAssigner}
import fr.renoux.gaston.model._
import fr.renoux.gaston.util.Context

import scala.util.Random


/** Use tabu search to improve a schedule. */
class TabuSearchImprover(
    val stopAtScore: Double = Double.MaxValue,
    val maxImprovementRounds: Int = 1000
)(implicit private val problem: Problem, ctx: Context) extends Improver.Base[TabuSearchImprover.State] {

  import TabuSearchImprover.State

  private val log = Logger[TabuSearchImprover]

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

    val neighboursPartial = navigator.neighbours(state.current).map(_._1)
      .filterNot(s => state.tabu.contains(s.planning))
      .distinctBy(_.planning)

    val neighborhood = for {
      partial <- neighboursPartial
      unimproved <- filler.fill(partial)(rand)
      improved = personImprover.improve(unimproved)
    } yield improved

    if (neighborhood.isEmpty) None else {
      val candidate = neighborhood.maxBy(_.score)
      val newTabu = state.tabu ++ neighborhood.map(_.planning)
      val newBest = if (candidate.score > state.best.score) candidate else state.best
      Some((newBest, State(best = newBest, current = candidate, tabu = newTabu)))
    }
  }

  /*
    @tailrec
    private def recImprove(
        best: Schedule,
        current: Schedule,
        tabu: Set[Map[Slot, Set[Topic]]],
        maxRounds: Int,
        stopAtScore: Double
    )(implicit rand: Random): Schedule =
      if (maxRounds == 0 || best.score.value >= stopAtScore) best else {
        log.debug(s"Size of tabu is ${tabu.size}")

        val neighborhood = for {
          (partial, _) <- navigator.neighbours(current)
          if !tabu.contains(partial.topicsPerSlot)
          unimproved <- filler.fill(partial)(rand)
          improved = personImprover.improve(unimproved)
        } yield improved

        if (neighborhood.isEmpty) best else {
          val newTabu = tabu ++ neighborhood.map(_.topicsPerSlot)
          val candidate = neighborhood.maxBy(_.score)
          val newBest = if (candidate.score > best.score) candidate else best
          recImprove(newBest, candidate, newTabu, maxRounds - 1, stopAtScore)
        }
      } */
}

object TabuSearchImprover {

  case class State(
      best: Schedule,
      current: Schedule,
      tabu: Set[Schedule.Planning]
  )

}
