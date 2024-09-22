package fr.renoux.gaston.model

import cats.implicits._
import fr.renoux.gaston.util.Context
import fr.renoux.gaston.util.Context.chrono

import scala.annotation.tailrec

/** Calculate scores on a given schedule. */
final class ScoreCalculator(schedule: Schedule)(implicit ctx: Context) {

  import ScoreCalculator._

  /** Score for each person, regardless of its weight. All personal scores are slot-level, so the whole computation is done per slot. */
  lazy val unweightedScoresByPerson: Map[Person, FlatScore] = chrono("ScoreCalculator > unweightedScoresByPerson") {
    val slotScheduleScores: List[Map[Person, FlatScore]] = schedule.slotSchedulesList.map(_.unweightedScoresByPerson)
    if (schedule.problem.baseScoreByPerson.nonEmpty) (schedule.problem.baseScoreByPerson :: slotScheduleScores).combineAll
    else slotScheduleScores.combineAll
  }

  /** Score for each person, divided by that person's weight */
  lazy val weightedScoresByPerson: Map[Person, FlatScore] =
    unweightedScoresByPerson.map { case (p, s) => p -> s / p.weight }

  // TODO should weight scores earlier, so as to only have weighted scores in there
  def personalScoreFrom(unweightedScoresByPerson: Map[Person, FlatScore]): FlatScore = {
    val weightedScores = unweightedScoresByPerson.toSeq.map { case (p, s) => s / p.weight }
    val scoreWeightedPersons = weightedScores.sorted.foldRight(0.0) { case (s, acc) => s.value + (acc / RankFactor) }
    // TODO sorted is a major (17%) hot-spot
    FlatScore(scoreWeightedPersons)
  }

  /** Score related to persons */
  lazy val personalScore: FlatScore = chrono("ScoreCalculator > personalScore") {
    personalScoreFrom(unweightedScoresByPerson)
  }

  /** There are some impersonal global-level preferences, so we have to calculate them in addition to the slot computation. */
  lazy val impersonalScore: FlatScore = chrono("ScoreCalculator > impersonalScore") {
    FlatScore.sum(schedule.slotSchedulesList)(_.impersonalScore) + impersonalGlobalPreferencesScore
  }

  /** Those preferences are impersonal in that they are not weighted by a person. But person repartition does matter! */
  lazy val impersonalGlobalPreferencesScore: FlatScore = globalPreferencesScoreRec(schedule.problem.impersonalGlobalLevelPreferencesList)

  @tailrec
  private def globalPreferencesScoreRec(prefs: List[Preference.GlobalLevel], sum: Double = 0): FlatScore = prefs match {
    case Nil => FlatScore(sum)
    case p :: ps =>
      val s = p.scoreSchedule(schedule)
      if (s.isNegativeInfinity) s else globalPreferencesScoreRec(ps, sum + s.value)
  }

  /** Score that solution for the current problem. Returns a global score prioritizing the score of the least satisfied
    * person, with the total score as a tie breaker. Personal scores are divided by the person's weight before
    * comparison. */
  lazy val globalScore: FlatScore = chrono("ScoreCalculator > globalScore") {
    if (impersonalScore.isNegativeInfinity) impersonalScore else {
      impersonalScore + personalScore
    }
  }

}

object ScoreCalculator {

  /** Factor by which someone is worth more than the person immediately before him */
  private val RankFactor: Double = 2
}
