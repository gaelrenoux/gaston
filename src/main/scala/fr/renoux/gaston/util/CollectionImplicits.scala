package fr.renoux.gaston.util

import scalaz.Monoid
import scalaz.Scalaz._

import scala.collection.IterableOps


object CollectionImplicits {

  implicit class RichIterableOps[A, CC[_]](val wrapped: IterableOps[A, CC, _]) extends AnyVal {
    def replace[B >: A](pf: PartialFunction[A, B]): CC[B] = wrapped.map(a => pf.applyOrElse(a, identity[A]))

    def zipWith[B](f: A => B): CC[(A, B)] = wrapped.map(a => a -> f(a))

    def cross[B](bs: Iterable[B]): CC[(A, B)] = for {
      a <- wrapped
      b <- bs
    } yield (a, b)
  }

  implicit class IterableEitherOps[A, B, CC[_]](val wrapped: IterableOps[Either[A, B], CC, _]) extends AnyVal {
    def unzipEither: (CC[A], CC[B]) = {
      val lefts = wrapped.collect { case Left(a) => a }
      val rights = wrapped.collect { case Right(b) => b }
      (lefts, rights)
    }
  }

  implicit class IterableIterableOps[A, CCI[_], CCO[_]](val wrapped: IterableOps[IterableOps[A, CCI, _], CCO, _]) extends AnyVal {
    def mapMap[B](f: A => B): CCO[CCI[B]] = wrapped.map(_.map(f))
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

    def zipByKeys[K1 >: K, V1](that: Map[K1, V1]): Map[K1, (Option[V], Option[V1])] = {
      val inWrapped: Map[K1, (Option[V], Option[V1])] = wrapped.map { case (k, v) => k -> (Some(v), that.get(k)) }
      val notInWrapped: Map[K1, (Option[V], Option[V1])] = (that.keySet -- wrapped.keySet).map { k => k -> (None, that.get(k)) }.toMap
      inWrapped ++ notInWrapped
    }
  }

  @inline final implicit class MonoidMapOps[K, V](val wrapped: Map[K, V])(implicit monoid: Monoid[V]) {

    def addByKeys[K1 >: K](that: Map[K1, V]): Map[K1, V] = {
      that ++ wrapped.map { case (k, v) => k -> that.get(k).fold(v)(monoid.append(v, _)) }
    }
  }

}
