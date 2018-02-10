package fr.renoux.gaston.util

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.util.CollectionImplicits._

/**
  * Tool to dispatch list elements in separate slots, ensuring the repartition is as fair as possible.
  */
object Dispatch {

  /** Split without any more constraint than having each part about the same size, given a number of available slots. */
  def equally(slotCount: Int) = new EqualDispatch(slotCount)

  def equallyWithMaxes(maxes: Seq[Option[Int]]) = new EqualDispatchWithMaxes(maxes)

  /**
    * Dispatches the elements in the list on the slots of about the same size (+/- 1).
    */
  class EqualDispatch private[Dispatch](slots: Int) {

    def apply(total: Int): Seq[Int] = {
      val slotBaseSize = total / slots
      val remainder = total % slots
      Seq.fill(slots)(slotBaseSize).zipWithIndex map { case (s, ix) =>
        if (ix < remainder) s + 1 else s
      }
    }

    def apply[A](list: List[A]): List[List[A]] = {
      val counts = apply(list.size)
      val (result, Nil) = list.takeChunks(counts)
      result
    }
  }

  class EqualDispatchWithMaxes private[Dispatch](maxes: Seq[Option[Int]]) {
    private val flattenedMaxes = maxes.flatten
    private val unoptionedMaxes = maxes.map(_.getOrElse(Int.MaxValue))
    private val slotCount = maxes.size
    private val maxTotal = if (maxes.contains(None)) Int.MaxValue else flattenedMaxes.sum
    private val minMax = flattenedMaxes.min


    /** Dispatches the value in the slots, trying to have values as equal as possible with the max being respected.
      * @return the repartition, and the remainder (if a complete repartition was not possible) */
    def apply(total: Int): (Seq[Int], Int) = {
      val possibleTotal = math.min(total, maxTotal)
      val startingValue = math.min(minMax, possibleTotal / slotCount)

      val values = Array.fill(slotCount)(startingValue)
      var left = possibleTotal - (startingValue * slotCount)
      var index = 0

      while (left > 0) {
        val current = values(index)
        if (current < unoptionedMaxes(index)) {
          values(index) = values(index) + 1
          left = left - 1
        }
        index = (index + 1) % slotCount
      }

      (values.toSeq, possibleTotal)
    }

    /** Dispatches the elements in the list on on the slots, respecting a max value for slots. There may be a remainder at the end. */
    def apply[A](seq: Seq[A]): (Seq[Seq[A]], Seq[A]) = {
      val (counts, _) = apply(seq.size)
      seq.takeChunks(counts)
    }

  }

}