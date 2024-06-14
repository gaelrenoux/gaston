package fr.renoux.gaston.util

import scala.annotation.tailrec

object StringImplicits {

  @inline implicit final class StringOps(wrapped: String) {
    /** Keep replacing elements in a String until it cannot be found anymore. */
    @inline def replaceRec(a: String, b: String): String = StringImplicits.replaceRec(wrapped, a, b)
  }

  @tailrec
  @inline private def replaceRec(string: String, a: String, b: String): String = {
    val result = string.replace(a, b)
    if (result == string) result
    else replaceRec(result, a, b)
  }

}
