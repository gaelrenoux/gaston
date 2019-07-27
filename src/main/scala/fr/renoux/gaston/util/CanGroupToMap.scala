package fr.renoux.gaston.util

import fr.renoux.gaston.util.CollectionImplicits._

/** Typeclass for collections than can be grouped to a Map */
trait CanGroupToMap[A[_]] {

  /** Converts a collection of couples to a map. */
  def groupToMap[B, C](a: A[(B, C)]): Map[B, A[C]]
}

object CanGroupToMap {

  implicit object SetCanGroupToMap extends CanGroupToMap[Set] {
    override def groupToMap[B, C](a: Set[(B, C)]): Map[B, Set[C]] = a.groupBy(_._1).mapValuesStrict(_.map(_._2))
  }

  implicit object ListCanGroupToMap extends CanGroupToMap[List] {
    override def groupToMap[B, C](a: List[(B, C)]): Map[B, List[C]] = a.groupBy(_._1).mapValuesStrict(_.map(_._2))
  }

  implicit object SeqCanGroupToMap extends CanGroupToMap[Seq] {
    override def groupToMap[B, C](a: Seq[(B, C)]): Map[B, Seq[C]] = a.groupBy(_._1).mapValuesStrict(_.map(_._2))
  }

  implicit object IterableCanGroupToMap extends CanGroupToMap[Iterable] {
    override def groupToMap[B, C](a: Iterable[(B, C)]): Map[B, Iterable[C]] = a.groupBy(_._1).mapValuesStrict(_.map(_._2))
  }

  implicit class CanGroupToMapOps[A[_], B, C](val wrapped: A[(B, C)]) extends AnyVal {
    /** Converts a collection of couples to a map. */
    def groupToMap(implicit gtm: CanGroupToMap[A]): Map[B, A[C]] = gtm.groupToMap(wrapped)
  }

}
