package fr.renoux.gaston.util

import scalaz.Scalaz._

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom


object CollectionImplicits {

  implicit class TraversableOps[A, R](val wrapped: TraversableLike[A, R]) extends AnyVal {

    def replace[B >: A, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[R, B, That]): That =
      wrapped.map(pf.orElse { case a => a })

    def zipWith[B, That](f: A => B)(implicit bf: CanBuildFrom[R, (A, B), That]): That =
      wrapped.map(a => a -> f(a))

  }

  implicit class TraversableEitherOps[A, B, R](val wrapped: TraversableLike[Either[A, B], R]) extends AnyVal {

    def unzipEither[ThatA, ThatB](implicit af: CanBuildFrom[R, A, ThatA], bf: CanBuildFrom[R, B, ThatB]): (ThatA, ThatB) = {
      val lefts: ThatA = wrapped.collect {
        case Left(a) => a
      }
      val rights: ThatB = wrapped.collect {
        case Right(b) => b
      }
      (lefts, rights)
    }

  }

  implicit class Traversable2Ops[A, RI, RO](val wrapped: TraversableLike[TraversableLike[A, RI], RO]) extends AnyVal {

    def mapMap[B, RIB, ROB](f: A => B)(implicit bfi: CanBuildFrom[RI, B, RIB], bfo: CanBuildFrom[RO, RIB, ROB]): ROB =
      wrapped.map(_.map(f))

  }

  implicit class MapOps[K, V](val wrapped: Map[K, V]) extends AnyVal {

    def updatedWith(k: K)(f: V => V): Map[K, V] = wrapped.alter(k)(_.map(f))

    def updatedWithOrElse(k: K)(f: V => V, v: => V): Map[K, V] = {
      val newValue = wrapped.get(k).map(f).getOrElse(v)
      wrapped.updated(k, newValue)
    }

    def toFormattedString: String = wrapped.map { case (key, value) =>
      s"$key: $value"
    }.mkString("\n")

    /** Unjike Scala's mapValues, this one is **not** lazily evaluated. */
    def mapValuesStrict[V1](f: V => V1): Map[K, V1] = wrapped.map { case (k, v) => k -> f(v) }

    def mapKeys[K1](f: K => K1): Map[K1, V] = wrapped.map { case (k, v) => f(k) -> v }

  }

}
