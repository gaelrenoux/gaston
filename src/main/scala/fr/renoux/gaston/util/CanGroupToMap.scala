package fr.renoux.gaston.util

import cats.Functor
import cats.implicits.*


/** Typeclass for collections than can be grouped to a Map. Needs a functor for F when grouping above the level of couples. */
trait CanGroupToMap[F[_]] {

  extension [A, B](fab: F[(A, B)]) {
    /** Converts a collection of couples to a map. */
    inline def groupToMap: Map[A, F[B]]
  }

  extension [A, B, C](fabc: F[(A, B, C)])(using ff: Functor[F]) {
    /** Converts a collection of triplets to a map of map. */
    inline def groupToMap: Map[A, Map[B, F[C]]] = {
      val firstGrouping: Map[A, F[(B, C)]] = fabc.map { case (a, b, c) => (a, (b, c)) }.groupToMap
      firstGrouping.mapValuesStrict(_.groupToMap)
    }
  }
}

object CanGroupToMap {
  inline def apply[F[_] : CanGroupToMap]: CanGroupToMap[F] = summon[CanGroupToMap[F]]

  given SetCanGroupToMap: CanGroupToMap[Set] with {
    extension [A, B](fab: Set[(A, B)]) {
      override inline def groupToMap: Map[A, Set[B]] = fab.groupBy(_._1).mapValuesStrict(_.map(_._2))
    }
  }

  given ListCanGroupToMap: CanGroupToMap[List] with {
    extension [A, B](fab: List[(A, B)]) {
      override inline def groupToMap: Map[A, List[B]] = fab.groupBy(_._1).mapValuesStrict(_.map(_._2))
    }
  }

  given SeqCanGroupToMap: CanGroupToMap[Seq] with {
    extension [A, B](fab: Seq[(A, B)]) {
      override inline def groupToMap: Map[A, Seq[B]] = fab.groupBy(_._1).mapValuesStrict(_.map(_._2))
    }
  }

  given IndexedSeqCanGroupToMap: CanGroupToMap[IndexedSeq] with {
    extension [A, B](fab: IndexedSeq[(A, B)]) {
      override inline def groupToMap: Map[A, IndexedSeq[B]] = fab.groupBy(_._1).mapValuesStrict(_.map(_._2))
    }
  }

  given IterableCanGroupToMap: CanGroupToMap[Iterable] with {
    extension [A, B](fab: Iterable[(A, B)]) {
      override inline def groupToMap: Map[A, Iterable[B]] = fab.groupBy(_._1).mapValuesStrict(_.map(_._2))
    }
  }

}
