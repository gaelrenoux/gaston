package fr.renoux.gaston.util


/** Simple loop for ranges */
inline def fastLoop(inline start: Int, inline until: Int, inline advance: Int => Int = _ + 1)(
    inline loopBody: Int => Any
): Unit = {
  var i = start
  while (i < until) {
    loopBody(i)
    i = advance(i)
  }
}
