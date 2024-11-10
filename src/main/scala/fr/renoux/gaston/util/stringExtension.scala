package fr.renoux.gaston.util

import scala.annotation.tailrec

extension (str: String) {
  /** Keep replacing elements in a String until it cannot be found anymore. */
  @tailrec def replaceRec(a: String, b: String): String = {
    val result = str.replace(a, b)
    if (result == str) result
    else result.replaceRec(a, b)
  }
}
