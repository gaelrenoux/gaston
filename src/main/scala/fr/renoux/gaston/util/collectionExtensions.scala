package fr.renoux.gaston.util

import scala.collection.{IterableOnceOps, IterableOps}


extension [A, CC[_], C <: CC[A]](it: IterableOnceOps[A, CC, C]) {

  inline def replace[B >: A](pf: PartialFunction[A, B]): CC[B] = it.map(a => pf.applyOrElse(a, identity[A]))

  inline def zipWith[B](f: A => B): CC[(A, B)] = it.map(a => a -> f(a))

  inline def cross[B](bs: Iterable[B]): CC[(A, B)] = for {
    a <- it
    b <- bs
  } yield (a, b)

  inline infix def x[B](bs: Iterable[B]): CC[(A, B)] = cross(bs)

  /** This is only meaningful if the iterable's map is not parallel. */
  inline def mapWithState[B, S](initalState: S)(f: (A, S) => (B, S)): (CC[B], S) = {
    var state = initalState
    it.map { a =>
      val (a2, s2) = f(a, state)
      state = s2
      a2
    } -> state
  }
}


extension [A, CC[_], C <: CC[A]](it: IterableOps[A, CC, C]) {

  /** Keeps all elements where the f argument gives the minimum value over the collection. */
  inline def filterMinBy[B: Ordering](f: A => B): C = if (it.isEmpty) it.filter(_ => true) else {
    val min = it.view.map(f).min
    val w: C = it.filter(a => f(a) == min)
    w
  }

}

final class LeftCollector[A] extends PartialFunction[Either[A, Any], A] {
  override def isDefinedAt(x: Either[A, Any]): Boolean = x.isLeft

  override def apply(x: Either[A, Any]): A =
    x.swap.getOrElse(throw new IllegalStateException(x.toString))
}

final class RightCollector[B] extends PartialFunction[Either[Any, B], B] {
  override def isDefinedAt(x: Either[Any, B]): Boolean = x.isRight

  override def apply(x: Either[Any, B]): B =
    x.getOrElse(throw new IllegalStateException(x.toString))
}

extension [A, B, CC[_]](it: IterableOps[Either[A, B], CC, ?]) {
  inline def unzipEither: (CC[A], CC[B]) = {
    val lefts = it.collect(new LeftCollector[A])
    val rights = it.collect(new RightCollector[B])
    (lefts, rights)
  }
}


extension [A, CCI[_], CCO[_]](itIt: IterableOps[IterableOps[A, CCI, ?], CCO, ?]) {
  inline def mapMap[B](f: A => B): CCO[CCI[B]] = itIt.map(_.map(f))
}
