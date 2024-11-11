package fr.renoux.gaston.util

/** Simple generic loop */
inline def fastLoop[A](inline start: A, inline condition: A => Boolean, inline advance: A => A)(inline loopBody: A => Any): Unit = {
  var a = start
  while (condition(a)) {
    loopBody(a)
    a = advance(a)
  }
}

/** Simple loop for ranges */
inline def fastLoop(inline start: Int, inline until: Int, inline advance: Int => Int = _ + 1)(inline loopBody: Int => Any): Unit = {
  var i = start
  while (i < until) {
    loopBody(i)
    i = advance(i)
  }
}
