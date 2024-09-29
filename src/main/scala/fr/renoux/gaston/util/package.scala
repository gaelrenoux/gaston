package fr.renoux.gaston

import scala.annotation.StaticAnnotation

package object util {

  final class testOnly extends StaticAnnotation

  final class immutable extends StaticAnnotation
}
