package fr.renoux.gaston.model

import fr.renoux.gaston.util.Context
import fr.renoux.gaston.util.Context.chrono

/** Scoring utilities for a given Problem. */
object Scorer {

  /** Factor by which someone is worth more than the person immediately before him */
  private val RankFactor: Double = 2

  /** Score that solution for the current problem. Returns a global score prioritizing the score of the least satisfied
    * person, with the total score as a tie breaker. Personal scores are divided by the person's weight before
    * comparison. */
  def score(solution: Schedule)(implicit ctx: Context): Score = chrono("Scorer > score") {

    def impersonalScore = chrono("Scorer > score > scoreOther") {
      solution.impersonalScore
    }

    def unweightedScoresByPerson = chrono("Scorer > score > scoresByPerson") {
      solution.unweightedScoresByPerson
    }

    calculateGlobalScore(impersonalScore, unweightedScoresByPerson)
  }

  def calculateGlobalScore(impersonalScore: Score, unweightedScoresByPerson: => Map[Person, Score]): Score =
    if (impersonalScore.isNegativeInfinity) impersonalScore else {
      val weightedScores = unweightedScoresByPerson.toSeq.map { case (p, s) => s / p.weight }
      val scoreWeightedPersons = weightedScores.sorted.foldRight(0.0) { case (s, acc) => s.value + (acc / RankFactor) }
      impersonalScore + Score(scoreWeightedPersons)
    }


  /** Score for each person, divided by that person's weight */
  def weightedScoresByPerson(solution: Schedule): Map[Person, Score] = {
    val scoresByPerson = solution.unweightedScoresByPerson
    scoresByPerson.map { case (p, s) => p -> s / p.weight }
  }

}
