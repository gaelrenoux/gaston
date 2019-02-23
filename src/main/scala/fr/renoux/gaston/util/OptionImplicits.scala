package fr.renoux.gaston.util


object OptionImplicits {

  implicit class SetOptionOps[A](val wrapped: Option[Set[A]]) extends AnyVal {
    def emptyOrContains(a: A): Boolean = wrapped.forall(_.contains(a))
    def flatContains(a: A): Boolean = wrapped.exists(_.contains(a))
  }

}
