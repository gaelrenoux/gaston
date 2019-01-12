package fr.renoux.gaston.util

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom


object CollectionImplicits {

  implicit class TraversableOps[A, R](val wrapped: TraversableLike[A, R]) extends AnyVal {

    def replace[B >: A, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[R, B, That]): That =
      wrapped.map(pf.orElse(PartialFunction(identity)))

    def zipWith[B, That](f: A => B)(implicit bf: CanBuildFrom[R, (A, B), That]): That =
      wrapped.map(a => a -> f(a))

  }

  implicit class MapOps[K, V](val wrapped: Map[K, V]) extends AnyVal {

    def toFormattedString: String = wrapped.map { case (key, value) =>
      s"$key: $value"
    }.mkString("\n")

    /** Unjike Scala's mapValues, this one is **not** lazily evaluated. */
    def mapValuesStrict[V1](f: V => V1): Map[K, V1] = wrapped.map { case (k, v) => k -> f(v) }

  }


}
