package fr.renoux.gaston.util

/**
 * Commodity class to have optional arguments on methods or classes without having to pass them as Some[A].
 */
final case class Opt[+A](toOption: Option[A]) extends AnyVal


object Opt {

  given AToOptConversion[A]: Conversion[A, Opt[A]] with {
    override def apply(a: A): Opt[A] = Opt(Some(a))
  }

  given OptToOptionConversion[A]: Conversion[Opt[A], Option[A]] with {
    override def apply(a: Opt[A]): Option[A] = a.toOption
  }

  val Missing: Opt[Nothing] = new Opt(None)
}
