package fr.renoux.gaston.util

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

sealed class Chrono(blocking: Boolean = false) {

  private val _times: mutable.Map[String, Long] = mutable.Map[String, Long]().withDefaultValue(0L)

  private val _counts: mutable.Map[String, Long] = mutable.Map[String, Long]().withDefaultValue(0L)

  @inline def apply[A](name: String)(a: => A): A = {
    val start = System.currentTimeMillis()
    val evaluated = a
    val duration = System.currentTimeMillis() - start
    store(name, duration)
    evaluated
  }

  private def store(name: String, time: Long): Unit =
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

  def timesPretty: String =
    times.toSeq.sortBy(_._1).map { case (k, v) => s"  $k: $v" }.mkString("(\n", "\n", "\n)")


  def counts: Map[String, Long] = synchronized {
    _counts.toMap
  }

  def countsPretty: String =
    counts.toSeq.sortBy(_._1).map { case (k, v) => s"  $k: $v" }.mkString("(\n", "\n", "\n)")

}

object Chrono {

  object NoOp extends Chrono {
    @inline override def apply[A](name: String)(a: => A): A = a

    override val times: Map[String, Long] = Map.empty

    override val timesPretty: String = ""

    override val counts: Map[String, Long] = Map.empty

    override val countsPretty: String = ""
  }

}
