package fr.renoux.gaston.util

import scala.util.Random

class Temp {

  inline def hello(a: String): Unit = println(a)

  inline def goodbye(inline a: String): Unit = println(a)


  def checkHello(): Unit = {
    hello(Random.nextString(3))
  }

  def checkHello2(): Unit = {
    val a = Random.nextString(3)
    println(a)
  }

  def checkGoodbye(): Unit = {
    goodbye(Random.nextString(3))
  }

  def checkGoodbye2(): Unit = {
    println(Random.nextString(3))
  }
}
