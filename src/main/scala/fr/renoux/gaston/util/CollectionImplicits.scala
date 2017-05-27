package fr.renoux.gaston.util

import scala.annotation.tailrec

/**
  * Created by gael on 07/05/17.
  */
object CollectionImplicits {

  implicit class CoupleSetOps[A, B](wrapped: Set[(A, B)]) {
    /** Converts a set of couples to a map of the first element to a set of the second. */
    def groupToMap: Map[A, Set[B]] = wrapped.groupBy(_._1).mapValues(_.map(_._2))
  }

  implicit class ListOps[A](wrapped: List[A]) {

    /** Takes multiple elements rather than one */
    def take(firstElementCount: Int, secondElementCount: Int, elementsCount: Int*): List[List[A]] = {
      val elts = firstElementCount :: secondElementCount :: elementsCount.toList
      recTake(elts, wrapped)._1
    }

    /** Takes multiple elements rather than one. */
    def take(elementsCount: Iterable[Int]): List[List[A]] = recTake(elementsCount.toList, wrapped)._1

    /** Takes multiple elements rather than one. Returns the elements taken and all elements left */
    def takeWithRemainder(elementsCount: Iterable[Int]): (List[List[A]], List[A]) = recTake(elementsCount.toList, wrapped)

    @tailrec
    private def recTake(elementsCount: List[Int], list: List[A], acc: List[List[A]] = Nil): (List[List[A]], List[A]) = elementsCount match {
      case Nil =>
        (acc.reverse, list)
      case h :: q =>
        val (left, right) = list.splitAt(h)
        recTake(q, right, left :: acc)
    }

  }

}
