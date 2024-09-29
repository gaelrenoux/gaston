package fr.renoux.gaston.model

package object constraints {

  /** Constraints are entirely hard-coded: there should never be a need to check it, because we cannot construct a
    * schedule that doesn't respect it. However, we still check them for sanity. */
  final class hardCoded extends scala.annotation.StaticAnnotation
}
