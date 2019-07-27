package fr.renoux.gaston.util

import fr.renoux.gaston.util.CollectionImplicits._
import scalaz.Functor
import scalaz.Scalaz._
import scalaz._


/** Typeclass for collections than can be grouped to a Map. Needs a functor for F when grouping above the level of couples. */
trait CanGroupToMap[F[_]] {

  /** Converts a collection of couples to a map. */
  def groupToMap[A, B](a: F[(A, B)]): Map[A, F[B]]

  /** Converts a collection of triplets to a map of map. */
  def groupToMap3[A, B, C](fabc: F[(A, B, C)])(implicit ff: Functor[F]): Map[A, Map[B, F[C]]] = {
    val firstGrouping: Map[A, F[(B, C)]] = groupToMap(fabc.map { case (a, b, c) => (a, (b, c)) })
    firstGrouping.mapValuesStrict(groupToMap)
  }
}

object CanGroupToMap {
  def apply[F[_] : CanGroupToMap]: CanGroupToMap[F] = implicitly[CanGroupToMap[F]]

  implicit object SetCanGroupToMap extends CanGroupToMap[Set] {
    override def groupToMap[B, C](a: Set[(B, C)]): Map[B, Set[C]] = a.groupBy(_._1).mapValuesStrict(_.map(_._2))
  }

  implicit object ListCanGroupToMap extends CanGroupToMap[List] {
    override def groupToMap[B, C](a: List[(B, C)]): Map[B, List[C]] = a.groupBy(_._1).mapValuesStrict(_.map(_._2))
  }

  implicit object SeqCanGroupToMap extends CanGroupToMap[Seq] {
    override def groupToMap[B, C](a: Seq[(B, C)]): Map[B, Seq[C]] = a.groupBy(_._1).mapValuesStrict(_.map(_._2))
  }

  implicit object IndexedSeqCanGroupToMap extends CanGroupToMap[IndexedSeq] {
    override def groupToMap[B, C](a: IndexedSeq[(B, C)]): Map[B, IndexedSeq[C]] = a.groupBy(_._1).mapValuesStrict(_.map(_._2))
  }

  implicit object IterableCanGroupToMap extends CanGroupToMap[Iterable] {
    override def groupToMap[B, C](a: Iterable[(B, C)]): Map[B, Iterable[C]] = a.groupBy(_._1).mapValuesStrict(_.map(_._2))
  }

  /** Operations on F of couples */
  trait CoupleOps[F[_], A, B] {
    def typeClassInstance: CanGroupToMap[F]

    def self: F[(A, B)]

    @inline def groupToMap: Map[A, F[B]] = typeClassInstance.groupToMap(self)
  }

  /** Operations on F of triplets */
  trait TripletOps[F[_], A, B, C] {
    def typeClassInstance: CanGroupToMap[F]

    def functorInstance: Functor[F]

    def self: F[(A, B, C)]

    @inline def groupToMap: Map[A, Map[B, F[C]]] = typeClassInstance.groupToMap3(self)(functorInstance)
  }

  object ops { // scalastyle:ignore object.name
    implicit def toCoupleOps[F[_] : CanGroupToMap, A, B](target: F[(A, B)]): CoupleOps[F, A, B] =
      new CoupleOps[F, A, B] {
        @inline val self: F[(A, B)] = target
        @inline val typeClassInstance: CanGroupToMap[F] = CanGroupToMap[F]
      }

    implicit def toTripletOps[F[_] : CanGroupToMap : Functor, A, B, C](target: F[(A, B, C)]): TripletOps[F, A, B, C] =
      new TripletOps[F, A, B, C] {
        @inline val self: F[(A, B, C)] = target
        @inline val typeClassInstance: CanGroupToMap[F] = CanGroupToMap[F]
        @inline val functorInstance: Functor[F] = Functor[F]
      }
  }

}
