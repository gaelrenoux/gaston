package fr.renoux.gaston.model

import cats.implicits._
import fr.renoux.gaston.util.Context
import fr.renoux.gaston.util.Context.chrono

import scala.annotation.tailrec

/** Calculate scores on a given schedule. */
final class ScoreCalculator(schedule: Schedule)(implicit ctx: Context) {

  import ScoreCalculator._

  /** Score for each person, regardless of its weight. All personal scores are slot-level, so the whole computation is done per slot. */
  lazy val unweightedScoresByPerson: Map[Person, Score] = chrono("ScoreCalculator > unweightedScoresByPerson") {
    schedule.slotSchedulesList.map(_.unweightedScoresByPerson).combineAll
  }

  /** Score for each person, divided by that person's weight */
  lazy val weightedScoresByPerson: Map[Person, Score] =
    unweightedScoresByPerson.map { case (p, s) => p -> s / p.weight }

  def personalScoreFrom(unweightedScoresByPerson: Map[Person, Score]): Score = {
    val weightedScores = unweightedScoresByPerson.toSeq.map { case (p, s) => s / p.weight }
    val scoreWeightedPersons = weightedScores.sorted.foldRight(0.0) { case (s, acc) => s.value + (acc / RankFactor) }
    // TODO sorted is a major (17%) hot-spot
    Score(scoreWeightedPersons)
  }

  /** Score related to persons */
  lazy val personalScore: Score = chrono("ScoreCalculator > personalScore") {
    personalScoreFrom(unweightedScoresByPerson)
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
