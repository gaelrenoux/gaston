package fr.renoux.gaston.util

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

class Chrono(val enabled: Boolean = true) {

  private val _times: mutable.Map[String, Long] = mutable.Map[String, Long]().withDefaultValue(0L)

  private val _counts: mutable.Map[String, Long] = mutable.Map[String, Long]().withDefaultValue(0L)

  def apply[A](name: String, blocking: Boolean = false)(a: => A): A = {
    val start = System.currentTimeMillis()
    val evaluated = a
    val duration = System.currentTimeMillis() - start
    store(name, duration, blocking)
    evaluated
  }

  def store(name: String, time: Long, blocking: Boolean = false): Unit =
  if (blocking) {
    val _ = synchronized {
      _times.put(name, _times(name) + time)
      _counts.put(name, _counts(name) + 1)
    }
  } else {
    val _ = Future {
      synchronized {
        _times.put(name, _times(name) + time)
        _counts.put(name, _counts(name) + 1)
      }
    }(ExecutionContext.global)
  }

  def times: Map[String, Long] = synchronized {
    _times.toMap
  }

  def counts: Map[String, Long] = synchronized {
    _counts.toMap
  }

}

object Chrono {
  val NoOp = new Chrono(enabled = false)
}