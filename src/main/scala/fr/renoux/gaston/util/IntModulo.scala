package fr.renoux.gaston.util

import scala.annotation.targetName

/** An Int considered only within some Modulo. Values are necessarily between 0 and the modulo - 1 */
opaque type IntModulo = Int

object IntModulo {
  opaque type Modulo = Int

  object Modulo {
    inline def apply(a: Int): Modulo = a
  }

  inline def zero: IntModulo = 0
  inline def one: IntModulo = 1
  inline def two(using m: Modulo): IntModulo = 2 % m
  inline def three(using m: Modulo): IntModulo = 3 % m
  inline def four(using m: Modulo): IntModulo = 4 % m
  inline def five(using m: Modulo): IntModulo = 5 % m
  inline def six(using m: Modulo): IntModulo = 6 % m
  inline def seven(using m: Modulo): IntModulo = 7 % m
  inline def eight(using m: Modulo): IntModulo = 8 % m
  inline def nine(using m: Modulo): IntModulo = 9 % m
  inline def ten(using m: Modulo): IntModulo = 10 % m

  inline def modulo[A](i: Int)(inline f: Modulo ?=> A): A = f(using i)

  extension (a: IntModulo) {
    inline def toInt: Int = a
  }

  extension (a: IntModulo)(using m: Modulo) {
    @targetName("IntPlusModulo")
    inline def +(b: IntModulo): IntModulo = (a + b) % m

    @targetName("IntModuloMinus")
    inline def -(b: IntModulo): IntModulo = (a + m - b) % m

    @targetName("IntModuloEquals")
    inline def ==(b: IntModulo): Boolean = a == b

    @targetName("IntModuloNotEquals")
    inline def !=(b: IntModulo): Boolean = a != b
  }
}
