package fr.renoux.gaston.util

/**
  * Commodity class to have optional arguments on methods or classes without having to pass them as Some[A].
  */
case class Opt[A](wrapped: Option[A]) extends AnyVal


object Opt {

  implicit def optionToOpt[A](o: Option[A]): Opt[A] = Opt(o)

  implicit def scalarToOpt[A](a: A): Opt[A] = Opt(Some(a))

  implicit def optToOption[A](o: Opt[A]): Option[A] = o.wrapped

}
