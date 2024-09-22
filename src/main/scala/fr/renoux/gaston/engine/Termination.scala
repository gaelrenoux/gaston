package fr.renoux.gaston.engine

import fr.renoux.gaston.model.FlatScore

import java.time.Instant

/**
  * This class defines on which condition schedule generation can terminate.
  * @param score Stop once you reach a schedule with this score.
  * @param count Maximum number of schedules we want to examine.
  * @param timeout A timestamp when to stop.
  */
final case class Termination( // TODO Rename to Controls, or maybe TerminationConditions? And add class doc.
    score: Option[FlatScore] = None,
    count: Option[Long] = None,
    timeout: Option[Instant] = None
) {

  lazy val intCount: Option[Int] = count.collect {
    case c: Long if c <= Int.MaxValue => c.toInt
  }

  private val timeoutMs = timeout.map(_.toEpochMilli)

  def reduceCount(delta: Long): Termination = copy(count = count.map(_ - delta))

  /** Returns true if the argument score has reached the desired score */
  def checkScore(s: FlatScore): Boolean = score.exists(_ <= s)

  /** Returns true if the argument count has reached the max count */
  def checkCount(c: Long): Boolean = count.exists(_ <= c)

  /** Returns true if the argument time has reached the timeout */
  def checkTimeout(now: Instant): Boolean = timeoutMs.exists(_ <= now.toEpochMilli)

  /** Returns true if the argument time has reached the timeout */
  def checkTimeout(nowMs: Long): Boolean = timeoutMs.exists(_ <= nowMs)

  /** Returns true if the current time has reached the timeout */
  def checkTimeout(): Boolean = timeoutMs.exists(_ <= System.currentTimeMillis())
}

object Termination {
  /** No termination: schedules will be generated until a natural end, or will never stop. */
  val Perpetual: Termination = Termination()
}
