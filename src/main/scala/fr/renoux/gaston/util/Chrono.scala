package fr.renoux.gaston.util

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

/** A class to measure how much time a piece of code is doing.
 * @param blocking If true, storing the time spent after the code is done is blocking. Otherwise, it's deported into a Future.
 * */
sealed class Chrono(blocking: Boolean = false) {

  private val _times: mutable.Map[String, Long] = mutable.Map[String, Long]().withDefaultValue(0L)

  private val _counts: mutable.Map[String, Long] = mutable.Map[String, Long]().withDefaultValue(0L)

  /** Time spent in the block of code as second-argument, is going to be stored with the name given. You can then access
   * all time spent using the `times` method. */
  @inline def apply[A](name: String)(a: => A): A = {
    val start = System.currentTimeMillis()
    val evaluated = a
    val duration = System.currentTimeMillis() - start
    store(name, duration)
    evaluated
  }

  /** Stores a time into the times and counts. */
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

  def timesTotal: Map[String, Long] = synchronized {
    _times.toMap
  }

  def timesTotalPretty: String =
    timesTotal.toSeq.sortBy(_._1).map { case (k, v) => s"  $k: $v" }.mkString("(\n", "\n", "\n)")


  def counts: Map[String, Long] = synchronized {
    _counts.toMap
  }

  def countsPretty: String =
    counts.toSeq.sortBy(_._1).map { case (k, v) => s"  $k: $v" }.mkString("(\n", "\n", "\n)")

  def timesAverage: Map[String, Double] = {
    val cs = counts
    timesTotal.map { case (name, time) =>
      name -> (time.toDouble / cs(name))
    }
  }

  def timesAveragePretty: String =
    timesAverage.toSeq.sortBy(_._1).map { case (k, v) => s"  $k: $v" }.mkString("(\n", "\n", "\n)")

}

object Chrono {

  object NoOp extends Chrono {
    @inline override def apply[A](name: String)(a: => A): A = a

    override val timesTotal: Map[String, Long] = Map.empty

    override val timesTotalPretty: String = ""

    override val counts: Map[String, Long] = Map.empty

    override val countsPretty: String = ""
  }

}
