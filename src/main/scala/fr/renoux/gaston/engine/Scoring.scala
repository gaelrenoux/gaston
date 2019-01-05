package fr.renoux.gaston.engine

import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Person, Schedule, Score}

class Scoring(problem: Problem) {

  /** Score a solution for the current problem: equal to the minimum score. Personal scores are divided by the person's
    * weight before comparison. The total score comes only as a tie breaker. */
  final def score(solution: Schedule): Score = {
    val individualScores = problem.preferences.toSeq.map(p => p -> p.score(solution))
    val scoresByPerson = individualScores.groupBy(_._1.person) mapValues (_.map(_._2).sum)

    val weightedScoresByPerson = scoresByPerson map { case (p, s) => s / p.weight }

    weightedScoresByPerson.min * 1000 + weightedScoresByPerson.sum
  }

  final def scoreMinimumOnly(solution: Schedule): Score = {
    val individualScores = problem.preferences.toSeq.map(p => p -> p.score(solution))
    val scoresByPerson = individualScores.groupBy(_._1.person) mapValues (_.map(_._2).sum)

    val weightedScoresByPerson = scoresByPerson map { case (p, s) => s / p.weight }

    weightedScoresByPerson.min
  }

  final def scoreSumOnly(solution: Schedule): Score = {
    val individualScores = problem.preferences.toSeq.map(p => p -> p.score(solution))
    val scoresByPerson = individualScores.groupBy(_._1.person) mapValues (_.map(_._2).sum)

    val weightedScoresByPerson = scoresByPerson map { case (p, s) => s / p.weight }

    weightedScoresByPerson.sum
  }

  final def unweightedScoresByPerson(solution: Schedule): Map[Person, Score] = {
    val individualScores = problem.preferences.toSeq.map(p => p -> p.score(solution))
    individualScores.groupBy(_._1.person) mapValues (_.map(_._2).sum)
  }

  final def weightedScoresByPerson(solution: Schedule): Map[Person, Score] = {
    unweightedScoresByPerson(solution) map { case (p, s) => p -> s / p.weight }
  }

}
