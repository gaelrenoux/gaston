package fr.renoux.gaston.util

import scala.reflect.ClassTag


/** Array extension */
extension [A](inline as: Array[A]) {
  inline def fastFoldLeft[B](inline init: B)(inline f: (B, A) => B): B = {
    var res = init
    val asl = as.length
    var ix = 0
    while (ix < asl) {
      res = f(res, as(ix))
      ix += 1
    }
    res
  }

  inline def fastFoldRight[B](inline init: B)(inline f: (A, B) => B): B = {
    var res = init
    val asl = as.length
    var ix = asl
    while (ix > 0) {
      ix -= 1
      res = f(as(ix), res)
    }
    res
  }

  inline def fastCount(inline f: A => Boolean): Int = {
    var res = 0
    val asl = as.length
    var ix = 0
    while (ix < asl) {
      if (f(as(ix))) {
        res += 1
      }
      ix += 1
    }
    res
  }

  inline def fastForeach(inline f: A => Unit): Unit = {
    val asl = as.length
    var ix = 0
    while (ix < asl) {
      f(as(ix))
      ix += 1
    }
  }

  inline def fastForeachWithIndex(inline f: (A, Int) => Unit): Unit = {
    val asl = as.length
    var ix = 0
    while (ix < asl) {
      f(as(ix), ix)
      ix += 1
    }
  }

  inline def fastMap[B: ClassTag](inline f: A => B): Array[B] = {
    val asl = as.length
    val result = new Array[B](asl)
    var ix = 0
    while (ix < asl) {
      result(ix) = f(as(ix))
      ix += 1
    }
    result
  }

  inline def fastFill(inline a: => A): Unit = {
    val asl = as.length
    var ix = 0
    while (ix < asl) {
      as(ix) = a
      ix += 1
    }
  }

  inline def fastTabulate(inline f: Int => A): Unit = {
    val asl = as.length
    var ix = 0
    while (ix < asl) {
      as(ix) = f(ix)
      ix += 1
    }
  }
}

/** Scala iterable extension */
extension [A](inline as: Iterable[A]) {

  inline def fastFoldLeft[B](inline init: B)(inline f: (B, A) => B): B = {
    var res = init
    val it = as.iterator
    while (it.hasNext) {
      res = f(res, it.next())
    }
    res
  }

  inline def fastCount(inline f: A => Boolean): Int = {
    var res = 0
    val it = as.iterator
    while (it.hasNext) {
      if (f(it.next())) {
        res += 1
      }
    }
    res
  }

  inline def fastForeach(inline f: A => Unit): Unit = {
    val it = as.iterator
    while (it.hasNext) {
      f(it.next())
    }
  }
}


