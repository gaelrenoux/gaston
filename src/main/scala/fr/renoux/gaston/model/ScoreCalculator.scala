package fr.renoux.gaston.model

import cats.implicits._
import fr.renoux.gaston.util.Context
import fr.renoux.gaston.util.Context.chrono

import scala.annotation.tailrec

/** Calculate scores on a given schedule. */
final class ScoreCalculator(val schedule: Schedule)(implicit ctx: Context) {

  import ScoreCalculator._

  private val problem = schedule.problem

  /** Score for each person, regardless of its weight. All personal scores are slot-level, so the whole computation is done per slot. */
  lazy val unweightedScoresByPersonId: Array[Score] = chrono("ScoreCalculator > unweightedScoresByPerson") {
    Score.combineArrays(schedule.slotSchedulesList.map(_.unweightedScoresByPersonId), problem.counts.persons)
  }

  /** Score for each person, divided by that person's weight */
  lazy val weightedScoresByPersonId: Array[Score] = {
    val result = Array.fill(problem.counts.persons)(Score.Zero)
    for (id <- unweightedScoresByPersonId.indices) {
      result(id) = unweightedScoresByPersonId(id) / problem.weightsByPersonId(id)
    }
    result
  }

  def personalScoreFrom(unweightedScoresByPersonId: Array[Score]): Score = {
    val rankedWeightedScores = Array.tabulate(problem.counts.persons) { i =>
      val weight = problem.weightsByPersonId(i)
      val score = unweightedScoresByPersonId(i)
      (score.value / weight.value)
    }.sorted
    // TODO sorted is a major (17%) hot-spot
    val scoreWeightedPersons = rankedWeightedScores.foldRight(0.0) { case (s, acc) => s + (acc / RankFactor) }
    Score(scoreWeightedPersons)
  }

  /** Score related to persons */
  lazy val personalScore: Score = chrono("ScoreCalculator > personalScore") {
    personalScoreFrom(unweightedScoresByPersonId)
  }

  /** There are some impersonal global-level preferences, so we have to calculate them in addition to the slot computation. */
  lazy val impersonalScore: Score = chrono("ScoreCalculator > impersonalScore") {
    schedule.slotSchedulesList.map(_.impersonalScore).sum + preferencesScoreRec(schedule.problem.impersonalGlobalLevelPreferencesList)
  }

  @tailrec
  private def preferencesScoreRec(prefs: List[Preference.GlobalLevel], sum: Double = 0): Score = prefs match {
    case Nil => Score(sum)
    case p :: ps =>
      val s = p.scoreSchedule(schedule)
      if (s.value == Double.NegativeInfinity) s else preferencesScoreRec(ps, sum + s.value)
  }

  /** Score that solution for the current problem. Returns a global score prioritizing the score of the least satisfied
    * person, with the total score as a tie breaker. Personal scores are divided by the person's weight before
    * comparison. */
  lazy val globalScore: Score = chrono("ScoreCalculator > globalScore") {
    if (impersonalScore.isNegativeInfinity) impersonalScore else {
      impersonalScore + personalScore
    }
  }

}

object ScoreCalculator {

  /** Factor by which someone is worth more than the person immediately before him */
  private val RankFactor: Double = 2
}
