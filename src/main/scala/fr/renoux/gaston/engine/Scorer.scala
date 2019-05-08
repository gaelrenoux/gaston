package fr.renoux.gaston.engine

import fr.renoux.gaston.model.{Person, Schedule, Score}

/** Scoring utilities for a given Problem. */
object Scorer {

  /** Factor by which someone is worth more than the person immediately before him */
  private val RankFactor: Double = 2

  /** Score that solution for the current problem. Returns a global score prioritizing the score of the least satisfied
    * person, with the total score as a tie breaker. Personal scores are divided by the person's weight before
    * comparison. */
  final def score(solution: Schedule): Score = {
    val scoresByPerson = solution.unweightedScoresByPerson
    val weightedScores = scoresByPerson.map { case (p, s) => s / p.weight }
    val scoreWeightedPersons = weightedScores.toSeq.sorted.foldRight(0.0) { case (s, acc) => s.value + (acc / RankFactor) }

    val scoreOther = solution.unpersonalScore.value
    Score(
      scoreOther + scoreWeightedPersons
    )
  }

  /** Score for each person, divided by that person's weight */
  final def weightedScoresByPerson(solution: Schedule): Map[Person, Score] = {
    val scoresByPerson = solution.unweightedScoresByPerson
    scoresByPerson.map { case (p, s) => p -> s / p.weight }
  }

}
