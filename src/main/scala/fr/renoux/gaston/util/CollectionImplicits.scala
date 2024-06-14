package fr.renoux.gaston.util

import mouse.map._

import scala.collection.IterableOps
import scala.collection.IterableOnceOps


/** A bunch of extension mehtods over collections. */
object CollectionImplicits {

  @inline implicit final class RichIterableOnceOps[A, CC[_], C <: CC[A]](val wrapped: IterableOnceOps[A, CC, C]) extends AnyVal {
    @inline def replace[B >: A](pf: PartialFunction[A, B]): CC[B] = wrapped.map(a => pf.applyOrElse(a, identity[A]))

    @inline def zipWith[B](f: A => B): CC[(A, B)] = wrapped.map(a => a -> f(a))

    @inline def cross[B](bs: Iterable[B]): CC[(A, B)] = for {
      a <- wrapped
      b <- bs
    } yield (a, b)

    @inline def mapWithState[B, S](initalState: S)(f: (A, S) => (B, S)): (CC[B], S) = {
      // TODO this assumes a sequential map, not a parallel map
      var state = initalState
      wrapped.map { a =>
        val (a2, s2) = f(a, state)
        state = s2
        a2
      } -> state
    }
  }

  @inline implicit final class RichIterableOps[A, CC[_], C <: CC[A]](val wrapped: IterableOps[A, CC, C]) extends AnyVal {

    /** Keeps all elements where the f argument gives the minimum value over the collection. */
    @inline def filterMinBy[B: Ordering](f: A => B): C = if (wrapped.isEmpty) wrapped.filter(_ => true) else {
      val min = wrapped.view.map(f).min
      val w: C = wrapped.filter(a => f(a) == min)
      w
    }
  }

  @inline implicit final class IterableEitherOps[A, B, CC[_]](val wrapped: IterableOps[Either[A, B], CC, _]) extends AnyVal {
    @inline def unzipEither: (CC[A], CC[B]) = {
      val lefts = wrapped.collect { case Left(a) => a }
      val rights = wrapped.collect { case Right(b) => b }
      (lefts, rights)
    }
  }

  @inline implicit final class IterableIterableOps[A, CCI[_], CCO[_]](val wrapped: IterableOps[IterableOps[A, CCI, _], CCO, _]) extends AnyVal {
    @inline def mapMap[B](f: A => B): CCO[CCI[B]] = wrapped.map(_.map(f))
  }

  @inline implicit final class MapOps[K, V](val wrapped: Map[K, V]) extends AnyVal {

    @inline def getMinKey(implicit o: Ordering[K]): Option[V] = if (wrapped.isEmpty) None else Some(wrapped.minBy(_._1)._2)

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
