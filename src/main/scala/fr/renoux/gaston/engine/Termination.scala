package fr.renoux.gaston.engine

import fr.renoux.gaston.model.Score

import java.time.Instant

/**
  * This class defines on which condition schedule generation can terminate.
  * @param score Stop once you reach a schedule with this score.
  * @param count Maximum number of schedules we want to examine.
  * @param timeout A timestamp when to stop.
  */
final case class Termination( // TODO Rename to Controls, or maybe TerminationConditions? And add class doc.
    score: Option[Score] = None,
    count: Option[Long] = None,
    timeout: Option[Instant] = None
) {
  def reduceCount(delta: Long): Termination = copy(count = count.map(_ - delta))

  lazy val intCount: Option[Int] = count.collect {
    case c: Long if c <= Int.MaxValue => c.toInt
  }
}

object Termination {
  /** No termination: schedules will be generated until a natural end, or will never stop. */
  val Perpetual: Termination = Termination()
}
