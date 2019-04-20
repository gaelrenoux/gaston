package fr.renoux.gaston.engine

import fr.renoux.gaston.model.{Problem, Schedule, Score, ScoredSchedule, Topic}
import fr.renoux.gaston.util.Tools

import scala.annotation.tailrec
import scala.collection.IterableView
import scala.util.Random

class Engine(
    problem: Problem,
    altImprover: Problem => ScheduleImprover = new GreedyScheduleImprover(_)
) {

  val generator: PartialSchedulesGenerator = new PartialSchedulesGenerator(problem)
  val filler: PartialScheduleFiller = new PartialScheduleFiller(problem)
  val improver: ScheduleImprover = altImprover(problem)

  /** Produces a schedule and its score */
  def run(seed: Long)(implicit tools: Tools): ScoredSchedule = {
    implicit val _r: Random = new Random(seed)

    val Some(unimproved) = generateUnimproved
    val unimprovedScore = Scorer.score(unimproved)

    val improved = improve(unimproved, unimprovedScore)
    val improvedScore = Scorer.score(improved)

    recSwapImprove(ScoredSchedule(improved, improvedScore))
  }

  /** For testing purposes */
  def generateUnimproved(implicit random: Random, ctx: Context, tools: Tools): Option[Schedule] =
    tools.chrono("ConstrainedScheduleFactory.makeSchedule") {
      generator.lazySeq.map(filler.fill).flatten.headOption
    }

  /** Improve the current schedule moving persons only. */
  private def improve(schedule: Schedule, score: Score)(implicit rand: Random, tools: Tools): Schedule =
    tools.chrono("ScheduleImprover.improve") {
      improver.improve(schedule, score)
    }

  /** Improve the schedule by trying swap after swap of topics. */
  @tailrec
  private def recSwapImprove(schedule: ScoredSchedule, previousSwap: Option[(Topic, Topic)] = None, maxRound: Int = 100)(implicit rand: Random, tools: Tools): ScoredSchedule =
    if (maxRound == 0) schedule else firstBetterSwap(schedule, previousSwap) match {
      case None => schedule //can't make it any better
      case Some((swappedSchedule, swap)) => recSwapImprove(swappedSchedule, Some(swap), maxRound - 1)
    }

  /** Take an already improved schedule, and return the first better schedule it can found by swapping topics. */
  private def firstBetterSwap(scoredSchedule: ScoredSchedule, previousSwap: Option[(Topic, Topic)])
    (implicit rand: Random, tools: Tools): Option[(ScoredSchedule, (Topic, Topic))] = {
    val ScoredSchedule(schedule, score) = scoredSchedule

    val allSwaps: IterableView[(ScoredSchedule, (Topic, Topic)), Iterable[_]] = for {
      /* Iterate over all topics on differing slots */
      (s1, s2) <- Random.shuffle(problem.slotCouples.toSeq).view //randomize for performances
      t1 <- schedule.topicsPerSlot(s1).view
      t2 <- schedule.topicsPerSlot(s2).view
      if !previousSwap.contains((t1, t2)) && !previousSwap.contains((t2, t1)) //not the swap we just did

      /* Filter out impossible swaps because of mandatory persons */
      mandatoryPersonsTopic1 = problem.mandatoryPersonsPerTopic(t1)
      mandatoryPersonsSlot2 = schedule.mandatoryPersonsOnSlot(s2)
      if !mandatoryPersonsTopic1.exists(mandatoryPersonsSlot2) //check mandatories of T1 are not already blocked on S2
      mandatoryPersonsTopic2 = problem.mandatoryPersonsPerTopic(t2)
      mandatoryPersonsSlot1 = schedule.mandatoryPersonsOnSlot(s1)
      if !mandatoryPersonsTopic2.exists(mandatoryPersonsSlot1) //check mandatories of T2 are not already blocked on S1

      /* Generate the swap and improve it as much as possible */
      partial = schedule.clearSlots(s1, s2).swapTopics(s1 -> t1, s2 -> t2)
      unimproved <- filler.fill(partial)
      unimprovedScore = Scorer.score(unimproved)
      improved = improve(unimproved, unimprovedScore)
      improvedScore = Scorer.score(improved)
      if improvedScore > score
    } yield (ScoredSchedule(improved, improvedScore), (t1, t2))

    allSwaps.headOption
  }
}
