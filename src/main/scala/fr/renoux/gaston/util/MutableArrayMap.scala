package fr.renoux.gaston.util

import scala.reflect.ClassTag

/** An mutable map where the key is only identified by its integer ID. You cannot get the keys back, you can just find
 * the associated value. Identifier can only go from 0 to its capacity (excluded).
 */
opaque type MutableArrayMap[A <: Identified, B] = Array[B]

object MutableArrayMap {
  extension [A <: Identified, B](am: MutableArrayMap[A, B]) {
    inline def capacity: Int = am.length

    inline def apply(inline a: A): B = am(a.id)

    inline def apply(inline i: Int): B = am(i)

    inline def update(inline a: A, inline b: B): Unit = {
      am(a.id) = b
    }

    inline def update(inline i: Int, inline b: B): Unit = {
      am(i) = b
    }

    inline def toArrayMap: ArrayMap[A, B] = ArrayMap.unsafeFrom(am)
    
    inline def unsafeContent: Array[B] = am
  }

  def fill[A <: Identified, B: ClassTag](size: Int, default: B): MutableArrayMap[A, B] = {
    Array.fill[B](size)(default)
  }
}


