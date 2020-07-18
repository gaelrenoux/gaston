package fr.renoux.gaston.util

object OptionImplicits {

  @inline final implicit class SetOptionOps[A](val wrapped: Option[Set[A]]) extends AnyVal {
    @inline def emptyOrContains(a: A): Boolean = wrapped.forall(_.contains(a))

    @inline def flatContains(a: A): Boolean = wrapped.exists(_.contains(a))
  }

  @inline final implicit class AnyWithOptionalOps[A](val a: A) extends AnyVal {
    @inline def optional[B](b: Option[B])(f: (A, B) => A): A = b.fold(a)(f(a, _))
  }

}
