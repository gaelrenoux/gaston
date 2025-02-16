package fr.renoux.gaston.util

import scala.util.Random


extension [A](as: Array[A]) {

  /** Shuffles elements in the array, using a Fisher-Yates-Durstenfeld shuffle */
  def shuffle(using rand: Random): Unit = {
    val asl = as.length
    fastLoop(asl - 1, _ > 0, _ - 1) { i =>
      val j = rand.nextInt(i + 1)
      val tmp = as(i)
      as(i) = as(j)
      as(j) = tmp
    }
  }

}
