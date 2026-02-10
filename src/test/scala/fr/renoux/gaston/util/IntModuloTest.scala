package fr.renoux.gaston.util

import fr.renoux.gaston.TestBase

class IntModuloTest extends TestBase {

  import IntModulo._

  "Int values" in {
    IntModulo.zero.toInt should be(0)
    IntModulo.one.toInt should be(1)

    modulo(5) {
      two should be(2)
      three should be(3)
      four should be(4)
      five should be(0)
      six should be(1)
      seven should be(2)
      eight should be(3)
      nine should be(4)
      ten should be(0)
    }

    modulo(64) {
      two should be(2)
      three should be(3)
      four should be(4)
      five should be(5)
      six should be(6)
      seven should be(7)
      eight should be(8)
      nine should be(9)
      ten should be(10)
    }

  }

  "Plus" in {
    val zero = IntModulo.zero
    val one = IntModulo.one

    modulo(5) {
      two + zero should be(two)
      zero + two should be(two)
      two + one should be(three)
      one + two should be(three)
      four + two should be(one)
      two + four should be(one)
    }

    modulo(64) {
      two + zero should be(two)
      zero + two should be(two)
      two + one should be(three)
      one + two should be(three)
      four + two should be(six)
      two + four should be(six)
    }
  }

  "Minus" in {
    val zero = IntModulo.zero
    val one = IntModulo.one

    modulo(5) {
      two - zero should be(two)
      zero - two should be(three)
      two - one should be(one)
      one - two should be(four)
      four - two should be(two)
      two - four should be(three)
      three - three should be(zero)
    }

    modulo(64) {
      two - zero should be(two)
      (zero - two).toInt should be(62)
      two - one should be(one)
      (one - two).toInt should be(63)
      four - two should be(two)
      (two - four).toInt should be(62)
      three - three should be(zero)
    }
  }

}
