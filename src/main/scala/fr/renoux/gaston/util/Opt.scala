package fr.renoux.gaston.util

/**
  * Created by gael on 07/05/17.
  */
case class Opt[A](wrapped: Option[A]) extends AnyVal


object Opt {

  implicit def optionToOpt[A](o: Option[A]): Opt[A] = Opt(o)

  implicit def scalarToOpt[A](a: A): Opt[A] = Opt(Some(a))

  implicit def optToOption[A](o: Opt[A]): Option[A] = o.wrapped

}
