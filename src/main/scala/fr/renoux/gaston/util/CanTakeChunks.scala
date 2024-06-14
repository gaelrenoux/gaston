package fr.renoux.gaston.util

import scala.annotation.tailrec


/** Typeclass for collections where elements can be taken by chunks rather than one by one */
trait CanTakeChunks[T[_] <: Iterable[_]] {

  /** Commodity method with varargs for `takeChunks(T[A], Iterable[Int])` */
  @inline
  def takeChunks[A](as: T[A], elementsCount: Int*): (T[T[A]], T[A]) = takeChunks(as, elementsCount)

  /** Takes chunks from the list, with a specific size for each chunk. Returns a collection of the chunks taken and all
    * elements left. */
  def takeChunks[A](as: T[A], elementsCount: Iterable[Int]): (T[T[A]], T[A])
}

object CanTakeChunks {

  implicit object ListCanTakeChunks extends CanTakeChunks[List] {

    @inline def takeChunks[A](as: List[A], elementsCount: Iterable[Int]): (List[List[A]], List[A]) =
      recTake(elementsCount.toList, as)

    @tailrec
    private def recTake[A](elementsCount: List[Int], as: List[A], acc: List[List[A]] = Nil): (List[List[A]], List[A]) =
      elementsCount match {
        case Nil =>
          (acc.reverse, as)
        case h :: q =>
          val (left, right) = as.splitAt(h)
          recTake(q, right, left :: acc)
      }
  }

  implicit object SeqCanTakeChunks extends CanTakeChunks[Seq] {

    @inline def takeChunks[A](as: Seq[A], elementsCount: Iterable[Int]): (Seq[Seq[A]], Seq[A]) =
      recTake(elementsCount.toList, as)

    @tailrec
    private def recTake[A](elementsCount: List[Int], list: Seq[A], acc: List[Seq[A]] = Nil): (Seq[Seq[A]], Seq[A]) =
      elementsCount match {
        case Nil =>
          (acc.reverse, list)
        case h :: q =>
          val (left, right) = list.splitAt(h)
          recTake(q, right, left :: acc)
      }
  }

  implicit final class Ops[T[_] <: Iterable[_], A](val wrapped: T[A]) extends AnyVal {

    /** Takes chunks from the list, with a specific size. */
    @inline
    def takeChunks(elementsCount: Int*)(implicit canTakeChunks: CanTakeChunks[T]): (T[T[A]], T[A]) =
      canTakeChunks.takeChunks(wrapped, elementsCount)

    /** Takes chunks from the list, with a specific size. Returns the elements taken and all elements left */
    @inline
    def takeChunks(elementsCount: Iterable[Int])(implicit canTakeChunks: CanTakeChunks[T]): (T[T[A]], T[A]) =
      canTakeChunks.takeChunks(wrapped, elementsCount)

  }

}
