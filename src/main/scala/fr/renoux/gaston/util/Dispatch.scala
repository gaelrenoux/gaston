package fr.renoux.gaston.util

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.util.RicherCollections._

import scala.collection.mutable

/**
  * Tool to dispatch list elements in separate slots, ensuring the repartition is as fair as possible.
  */
object Dispatch {

  private val log = Logger(Dispatch.getClass)

  def equally(slotCount: Int) = new EqualDispatch(slotCount)

  def equallyWithMaxes(maxes: Seq[Int]) = new EqualDispatchWithMaxes(maxes)

  /**
    * Dispatches the elements in the list on the slots of about the same size (+/- 1).
    */
  class EqualDispatch private[Dispatch](slots: Int) {

    def apply[A](list: List[A]): List[List[A]] = {
      val itemCount = list.size
      val slotBaseSize = itemCount / slots
      val remainder = itemCount % slots
      val slotSizes = Seq.fill(slots)(slotBaseSize).zipWithIndex map { case (s, ix) =>
        if (ix < remainder) s + 1 else s
      }
      val (result, Nil) = list.takeChunks(slotSizes)
      result
    }
  }

  /** Dispatches the elements in the list on on the slots, respecting a max value for slots. There may be a remainder at the end. */
  class EqualDispatchWithMaxes private[Dispatch](maxes: Seq[Int]) {
    private val slotCount = maxes.size

    def apply[A](seq: Seq[A]): (Seq[Seq[A]], Seq[A]) = {
      val itemCount = seq.size
      /* sometimes maxes are set at max int, no need to go that high */
      val realMaxes = maxes map (math.min(_, itemCount))

      val sizes = mutable.Seq(realMaxes: _*)


      while (sizes.sum > itemCount) {
        log.trace(s"Sizes are $sizes")
        val max = sizes.max
        var index = slotCount - 1
        while (sizes.sum > itemCount && index >= 0) {
          val s = sizes(index)
          if (s == max) sizes(index) = s - 1
          index = index - 1
        }
      }
      log.trace(s"Sizes are $sizes")

      seq.takeChunks(sizes)
    }

  }

}