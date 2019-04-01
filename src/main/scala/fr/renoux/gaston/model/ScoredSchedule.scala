package fr.renoux.gaston.model


case class ScoredSchedule(schedule: Schedule, score: Score) extends Ordered[ScoredSchedule] {
  override def compare(that: ScoredSchedule): Int = score.compare(that.score)
}

object ScoredSchedule {
  def empty(implicit problem: Problem) = ScoredSchedule(Schedule.empty, Score.MinValue)
}