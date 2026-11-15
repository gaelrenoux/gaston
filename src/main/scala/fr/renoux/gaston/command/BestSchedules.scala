package fr.renoux.gaston.command

import fr.renoux.gaston.model.{Schedule, Score}

import scala.math.Ordering.Implicits.infixOrderingOps

/** Keeps the best schedules found so far, at most one per schedule chain (identified by its chain-seed). Successive
 * schedules on the same chain are typically very close to each other, so keeping only the best one for each chain
 * gives more diverse alternatives.
 *
 * Immutable class: any change results in a new instance.
 *
 * @param capacity Maximum number of schedules kept.
 * @param schedules Schedules kept, ordered by descending score. Never contains two schedules with the same chain-seed.
 */
final case class BestSchedules private(capacity: Int, schedules: Vector[Schedule]) {

  /** Score of the worst schedule kept, if this is full. Any schedule from a new chain must be better to be kept. */
  private lazy val threshold: Score =
    if (schedules.size < capacity) Score.NegativeInfinity else schedules.last.score

  /** Best schedule kept, if any. */
  def best: Option[Schedule] = schedules.headOption

  /** Returns a new instance with that schedule added, if it is good enough to be kept. If a schedule from the same
   * chain is already kept, only the best of both is kept. */
  def add(schedule: Schedule): BestSchedules = {
    val sameChainIndex = schedules.indexWhere(_.chainSeed == schedule.chainSeed)
    val isKept =
      if (sameChainIndex >= 0) schedule.score > schedules(sameChainIndex).score
      else schedule.score > threshold
    if (!isKept) this else {
      val others = if (sameChainIndex >= 0) schedules.patch(sameChainIndex, Nil, 1) else schedules
      val (better, worse) = others.span(_.score >= schedule.score)
      copy(schedules = ((better :+ schedule) ++ worse).take(capacity))
    }
  }

  /** Returns a new instance with all the schedules kept in that instance added. Capacity is the one of this.*/
  def merge(that: BestSchedules): BestSchedules = that.schedules.foldLeft(this)(_.add(_))
}

object BestSchedules {
  def empty(capacity: Int): BestSchedules = {
    require(capacity >= 1, "Capacity must be at least 1")
    BestSchedules(capacity, Vector.empty)
  }
}
