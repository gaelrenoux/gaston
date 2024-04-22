package fr.renoux.gaston.util

/** A bunch of extension mehtods regarding options. */
object OptionImplicits {

  @inline final implicit class AnyWithOptionalOps[A](val a: A) extends AnyVal {
    @inline def optional[B](b: Option[B])(f: (A, B) => A): A = b.fold(a)(f(a, _))
  }

}
