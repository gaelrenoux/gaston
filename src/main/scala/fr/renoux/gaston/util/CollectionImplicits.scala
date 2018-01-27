package fr.renoux.gaston.util

import scala.annotation.tailrec

/**
  * Created by gael on 07/05/17.
  */
object CollectionImplicits {

  
  trait GroupableToMap[A[_]] {
    /** Converts a set of couples to a map of the first element to a set of the second. */
    def groupToMap[B, C](a: A[(B, C)]): Map[B, A[C]]
  }

  implicit object SetIsGroupableToMap extends GroupableToMap[Set] {
    /** Converts a set of couples to a map of the first element to a set of the second. */
    override def groupToMap[B, C](a: Set[(B, C)]): Map[B, Set[C]] = a.groupBy(_._1).mapValues(_.map(_._2))
  }

  implicit object ListIsGroupableToMap extends GroupableToMap[List] {
    /** Converts a set of couples to a map of the first element to a set of the second. */
    override def groupToMap[B, C](a: List[(B, C)]): Map[B, List[C]] = a.groupBy(_._1).mapValues(_.map(_._2))
  }

  implicit class GroupableToMapOps[A[_], B, C](val wrapped: A[(B, C)]) extends AnyVal {
    def groupToMap(implicit gtm: GroupableToMap[A]): Map[B, A[C]] = gtm.groupToMap(wrapped)
  }


  trait CanTakeChunks[T[_] <: Traversable[_]] {

    /** Takes chunks from the list, with a specific size. */
    def takeChunks[A](as: T[A], elementsCount: Int*): (T[T[A]], T[A]) = takeChunks(as, elementsCount)

    /** Takes chunks from the list, with a specific size. Returns the elements taken and all elements left */
    def takeChunks[A](as: T[A], elementsCount: Iterable[Int]): (T[T[A]], T[A])

    (1 :: Nil).take(3)
  }

  implicit object ListCanTakeChunks extends CanTakeChunks[List] {

    /** Takes chunks from the list, with a specific size. Returns the elements taken and all elements left. */
    def takeChunks[A](as: List[A], elementsCount: Iterable[Int]): (List[List[A]], List[A]) = recTake(elementsCount.toList, as)

    @tailrec
    private def recTake[A](elementsCount: List[Int], as: List[A], acc: List[List[A]] = Nil): (List[List[A]], List[A]) = elementsCount match {
      case Nil =>
        (acc.reverse, as)
      case h :: q =>
        val (left, right) = as.splitAt(h)
        recTake(q, right, left :: acc)
    }
  }

  implicit object SeqCanTakeChunks extends CanTakeChunks[Seq] {

    /** Takes chunks from the list, with a specific size. Returns the elements taken and all elements left. */
    def takeChunks[A](as: Seq[A], elementsCount: Iterable[Int]): (Seq[Seq[A]], Seq[A]) = recTake(elementsCount.toList, as)

    @tailrec
    private def recTake[A](elementsCount: List[Int], list: Seq[A], acc: List[Seq[A]] = Nil): (Seq[Seq[A]], Seq[A]) = elementsCount match {
      case Nil =>
        (acc.reverse, list)
      case h :: q =>
        val (left, right) = list.splitAt(h)
        recTake(q, right, left :: acc)
    }
  }

  implicit class CanTakeChunksOps[T[_] <: Traversable[_], A](val wrapped: T[A]) extends AnyVal {

    /** Takes chunks from the list, with a specific size. */
    def takeChunks(elementsCount: Int*)(implicit canTakeChunks: CanTakeChunks[T]): (T[T[A]], T[A]) = canTakeChunks.takeChunks(wrapped, elementsCount)

    /** Takes chunks from the list, with a specific size. Returns the elements taken and all elements left */
    def takeChunks(elementsCount: Iterable[Int])(implicit canTakeChunks: CanTakeChunks[T]): (T[T[A]], T[A]) = canTakeChunks.takeChunks(wrapped, elementsCount)

  }

  /*
  implicit class SeqOps[A, SeqType[_] <: Seq[_]](val wrapped: SeqType[A] with SeqLike[A, SeqType[A]]) extends AnyVal {

    private type Input = SeqType[A] with SeqLike[A, SeqType[A]]
    private type Output = SeqLike[SeqType[A], SeqType[SeqType[A]]] with SeqType[SeqType[A]]

    private def emptyOutput(implicit cb: CanBuildFrom[Output, Input, Output]): Output = cb().result()

    /** Takes chunks from the list, with a specific size. */
    def take(firstElementCount: Int, secondElementCount: Int, elementsCount: Int*)
            (implicit cb: CanBuildFrom[Output, SeqType[A], Output]): Output = {
      val elts = firstElementCount :: secondElementCount :: elementsCount.toList
      recTake(elts, wrapped, emptyOutput)._1
    }

    /** Takes chunks from the list, with a specific size. */
    def take(elementsCount: Iterable[Int])
            (implicit cb: CanBuildFrom[Output, SeqType[A], Output]): Output =
      recTake(elementsCount.toList, wrapped, emptyOutput)._1

    /** Takes chunks from the list, with a specific size. Returns the elements taken and all elements left */
    def takeWithRemainder(elementsCount: Iterable[Int])
                         (implicit cb: CanBuildFrom[Output, SeqType[A], Output]): (Output, SeqType[A]) =
      recTake(elementsCount.toList, wrapped, emptyOutput)

    @tailrec
    private def recTake(elementsCount: List[Int], seq: Input, acc: Output)
                       (implicit cb: CanBuildFrom[Output, SeqType[A], Output]): (Output, SeqType[A]) =
      elementsCount match {
        case Nil =>
          (acc, seq: SeqType[A])
        case h :: q =>
          val (left, right) = seq.splitAt(h)
          val newAcc = {
            val b = cb(acc)
            b += left
            b.result()
          }
          recTake(q, right, newAcc)
      }

  } */

}
