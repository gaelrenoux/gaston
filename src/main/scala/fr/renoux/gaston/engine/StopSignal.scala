package fr.renoux.gaston.engine

/** A signal that can be raised from any thread to request that schedule generation stops as soon as possible, in a
 * graceful way (runners will return the best schedules found so far).
 *
 * This class is mutable, as its whole point is to be raised by one thread and observed by others. It is thread-safe.
 */
final class StopSignal {

  @volatile private var requested: Boolean = false

  /** Requests a stop. Calling it multiple times has no additional effect. */
  def request(): Unit = {
    requested = true
  }

  /** Returns true if a stop has been requested. */
  def isRequested: Boolean = requested
}
