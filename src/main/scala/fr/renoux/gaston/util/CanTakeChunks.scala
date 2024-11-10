package fr.renoux.gaston.util

import scala.annotation.tailrec


/** Typeclass for collections where elements can be taken by chunks rather than one by one */
trait CanTakeChunks[F[_] <: Iterable[?]] {

  extension [A](as: F[A]) {
    /** Commodity method with varargs for `takeChunksFrom(Iterable[Int])` */
    inline def takeChunks(elementsCount: Int*): (F[F[A]], F[A]) = takeChunksWith(elementsCount)

    /** Takes chunks from the list, with a specific size for each chunk. Returns a collection of the chunks taken and all
     * elements left. */
    inline def takeChunksWith(elementsCount: Iterable[Int]): (F[F[A]], F[A])
  }
}

object CanTakeChunks {

  given ListCanTakeChunks: CanTakeChunks[List] with {
    extension [A](as: List[A]) {
      override inline def takeChunksWith(elementsCount: Iterable[Int]): (List[List[A]], List[A]) =
        recTake(elementsCount.toList, as)

      @tailrec
      private def recTake(elementsCount: List[Int], list: List[A], acc: List[List[A]] = Nil): (List[List[A]], List[A]) =
        elementsCount match {
          case Nil =>
            (acc.reverse, list)
          case h :: q =>
            val (left, right) = list.splitAt(h)
            recTake(q, right, left :: acc)
        }
    }
  }

  given SeqCanTakeChunks: CanTakeChunks[Seq] with {
    extension [A](as: Seq[A]) {

      override inline def takeChunksWith(elementsCount: Iterable[Int]): (Seq[Seq[A]], Seq[A]) =
        recTake(elementsCount.toList, as)

      @tailrec
      private def recTake(elementsCount: List[Int], seq: Seq[A], acc: List[Seq[A]] = Nil): (Seq[Seq[A]], Seq[A]) =
        elementsCount match {
          case Nil =>
            (acc.reverse, seq)
          case h :: q =>
            val (left, right) = seq.splitAt(h)
            recTake(q, right, left :: acc)
        }
    }
  }

}
