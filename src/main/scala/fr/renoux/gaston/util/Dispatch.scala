package fr.renoux.gaston.util

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.util.CollectionImplicits._

import scala.collection.mutable

object Dispatch {

  private val log = Logger(Dispatch.getClass)

  def equally(slots: Int) = new EqualDispatch(slots)

  def equallyWithMaxes(maxes: Seq[Int]) = new EqualDispatchWithMaxes(maxes)

  /**
    * Created by gael on 27/05/17.
    */
  class EqualDispatch private[Dispatch](slots: Int) {

    def apply[A](list: List[A]): List[List[A]] = {
      val total = list.size
      val baseSize = total / slots
      val remainder = total % slots
      val sizes = Seq.fill(slots)(baseSize).zipWithIndex map { case (s, ix) =>
        if (ix < remainder) s + 1 else s
      }
      list.take(sizes)
    }
  }

  class EqualDispatchWithMaxes private[Dispatch](maxes: Seq[Int]) {
    private val slots = maxes.size

    def apply[A](list: List[A]): (List[List[A]], List[A]) = {
      val total = list.size
      /* sometimes maxes are set at max int, no need to go that high */
      val realMaxes = maxes map (math.min(_, total))

      val sizes = mutable.Seq(realMaxes: _*)


      while (sizes.sum > total) {
        log.trace(s"Sizes are $sizes")
        val max = sizes.max
        var index = slots - 1
        while (sizes.sum > total && index >= 0) {
          val s = sizes(index)
          if (s == max) sizes(index) = s - 1
          index = index - 1
        }
      }
      log.trace(s"Sizes are $sizes")

      list.takeWithRemainder(sizes)
    }

  }

}