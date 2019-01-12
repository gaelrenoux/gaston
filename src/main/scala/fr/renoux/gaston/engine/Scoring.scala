package fr.renoux.gaston.engine

import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Person, Schedule, Score}
import fr.renoux.gaston.util.CollectionImplicits._

/** Scoring utilities for a given Problem. */
class Scoring(problem: Problem) {

  /** Score that solution for the current problem. Returns a global score prioritizing the score of the least satisfied
    * person, with the total score as a tie breaker. Personal scores are divided by the person's weight before
    * comparison. */
  final def score(solution: Schedule): Score = {
    val (min, total) = scorePair(solution)
    globalScore(min, total)
  }

  /** Gives the detailed score information for that solution: global score as calculated with [[score]], score of the
    * least satisfied person, and sum of all personal scores. Personal scores are divided by the person's weight before
    * comparison. */
  final def scoreDetailed(solution: Schedule): Scoring.Detail = {
    val (min, total) = scorePair(solution)
    Scoring.Detail(globalScore(min, total), min, total)
  }

  /** First is the score of the least satisfied person, second is the sum of all personal scores. On both, person weight
    * has been applied. */
  private def scorePair(solution: Schedule): (Score, Score) = {
    val scoresByPerson = unweightedScoresByPerson(solution)
    val weightedScores = scoresByPerson.map { case (p, s) => s / p.weight }

    (weightedScores.min, weightedScores.sum)
  }

  /** Score for each person, divided by that person's weight */
  final def weightedScoresByPerson(solution: Schedule): Map[Person, Score] = {
    val scoresByPerson = unweightedScoresByPerson(solution)
    scoresByPerson.map { case (p, s) => p -> s / p.weight }
  }

  private def globalScore(min: Score, sum: Score) = min * 1000 + sum

  /** Score for each person, regardless of its weight. */
  final def unweightedScoresByPerson(solution: Schedule): Map[Person, Score] = {
    val individualScores = problem.preferences.toSeq.map(p => p -> p.score(solution))
    individualScores.groupBy(_._1.person).mapValuesStrict (_.map(_._2).sum)
  }

}

object Scoring {

  case class Detail(global: Score, min: Score, sum: Score)

}
