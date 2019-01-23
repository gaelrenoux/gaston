package fr.renoux.gaston.engine

import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Person, Schedule, Score}
import fr.renoux.gaston.util.CollectionImplicits._

/** Scoring utilities for a given Problem. */
class Scorer private(problem: Problem) {

  /** Factor by which someone is worth more than the person immediately before him */
  private val RankFactor: Double = 2

  /** Score that solution for the current problem. Returns a global score prioritizing the score of the least satisfied
    * person, with the total score as a tie breaker. Personal scores are divided by the person's weight before
    * comparison. */
  final def score(solution: Schedule): Score = {
    val scoresByPerson = unweightedScoresByPerson(solution)
    val weightedScores = scoresByPerson.map { case (p, s) => s / p.weight }
    globalScore(weightedScores.toSeq)
  }

  /** Score for each person, divided by that person's weight */
  final def weightedScoresByPerson(solution: Schedule): Map[Person, Score] = {
    val scoresByPerson = unweightedScoresByPerson(solution)
    scoresByPerson.map { case (p, s) => p -> s / p.weight }
  }

  private def globalScore(weightedScores: Seq[Score]): Score = Score(
    weightedScores.sorted.foldRight(0.0){ case (s, acc) => s.value + (acc / RankFactor) }
  )

  /** Score for each person, regardless of its weight. */
  final def unweightedScoresByPerson(solution: Schedule): Map[Person, Score] = {
    val individualScores = problem.preferences.toSeq.map(p => p -> p.score(solution))
    individualScores.groupBy(_._1.person).mapValuesStrict(_.map(_._2).sum)
  }

}

object Scorer {

  case class Detail(global: Score, min: Score, sum: Score)

  def of(problem: Problem) = new Scorer(problem)

}
