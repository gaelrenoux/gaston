package fr.renoux.gaston.util

import mouse.map._

import scala.collection.IterableOps


object CollectionImplicits {

  @inline final implicit class RichIterableOps[A, CC[_]](val wrapped: IterableOps[A, CC, _]) extends AnyVal {
    @inline def replace[B >: A](pf: PartialFunction[A, B]): CC[B] = wrapped.map(a => pf.applyOrElse(a, identity[A]))

    @inline def zipWith[B](f: A => B): CC[(A, B)] = wrapped.map(a => a -> f(a))

    @inline def cross[B](bs: Iterable[B]): CC[(A, B)] = for {
      a <- wrapped
      b <- bs
    } yield (a, b)
  }

  @inline final implicit class IterableEitherOps[A, B, CC[_]](val wrapped: IterableOps[Either[A, B], CC, _]) extends AnyVal {
    @inline def unzipEither: (CC[A], CC[B]) = {
      val lefts = wrapped.collect { case Left(a) => a }
      val rights = wrapped.collect { case Right(b) => b }
      (lefts, rights)
    }
  }

  @inline final implicit class IterableIterableOps[A, CCI[_], CCO[_]](val wrapped: IterableOps[IterableOps[A, CCI, _], CCO, _]) extends AnyVal {
    @inline def mapMap[B](f: A => B): CCO[CCI[B]] = wrapped.map(_.map(f))
  }

  @inline final implicit class MapOps[K, V](val wrapped: Map[K, V]) extends AnyVal {

    @inline def updatedWith(k: K)(f: V => V): Map[K, V] = wrapped.updateAtKey(k, f)

    @inline def updatedWithOrElse(k: K)(f: V => V, v: => V): Map[K, V] = {
      val newValue = wrapped.get(k).map(f).getOrElse(v)
      wrapped.updated(k, newValue)
    }

    @inline def toFormattedString: String = wrapped.map { case (key, value) =>
      s"$key: $value"
    }.mkString("\n")

    /** Unjike Scala's mapValues, this one is **not** lazily evaluated. */
    @inline def mapValuesStrict[V1](f: V => V1): Map[K, V1] = wrapped.map { case (k, v) => k -> f(v) }

    @inline def zipByKeys[K1 >: K, V1](that: Map[K1, V1]): Map[K1, (Option[V], Option[V1])] = {
      val inWrapped: Map[K1, (Option[V], Option[V1])] = wrapped.map { case (k, v) => k -> (Some(v), that.get(k)) }
      val notInWrapped: Map[K1, (Option[V], Option[V1])] = (that.keySet -- wrapped.keySet).map { k => k -> (None, that.get(k)) }.toMap
      inWrapped ++ notInWrapped
    }
  }

}
