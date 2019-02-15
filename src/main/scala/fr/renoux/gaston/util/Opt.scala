package fr.renoux.gaston.util

/**
  * Commodity class to have optional arguments on methods or classes without having to pass them as Some[A].
  */
case class Opt[+A](toOption: Option[A]) extends AnyVal


object Opt {

  implicit def from[A](a: A): Opt[A] = Opt(Some(a))

  implicit def unwrap[A](o: Opt[A]): Option[A] = o.toOption

  val Missing: Opt[Nothing] = new Opt(None)
}
