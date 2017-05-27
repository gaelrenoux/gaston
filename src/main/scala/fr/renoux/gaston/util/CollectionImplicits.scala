package fr.renoux.gaston.util

/**
  * Created by gael on 07/05/17.
  */
object CollectionImplicits {

  implicit class CoupleSetOps[A, B](wrapped: Set[(A, B)]) {
    /** Converts a set of couples to a map of the first element to a set of the second. */
    def groupToMap: Map[A, Set[B]] = wrapped.groupBy(_._1).mapValues(_.map(_._2))
  }

}
