package fr.renoux.gaston.util

import scala.util.Random


extension [A](as: Array[A]) {
  
    /** Shuffles elements in the array */
    def shuffle(using rand: Random) = {
      val asl = as.length
      fastLoop(asl - 1, 1, _- 1) { i =>
        val j = rand.nextInt(i + 1)
        val tmp = as(i)
        as(i) = as(j)
        as(j) = tmp
      }
    }

}
