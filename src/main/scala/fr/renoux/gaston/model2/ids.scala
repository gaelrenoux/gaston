package fr.renoux.gaston.model2

import fr.renoux.gaston.util.*

opaque type Id >: Int = Int

object Id {
  extension (id: Id) {
    inline def value: Int = id
  }
  inline def None: Id = Int.MinValue
}

opaque type Count[+I <: Id] >: Int = Int

object Count {
  extension [I >: Int <: Id](c: Count[I]) {
    inline def value: Int = c

    inline def foreach(inline f: I => Unit): Unit = fastLoop(0, c)(f)

  }

  def apply[I <: Id](i: Int): Count[I] = i

  val Zero: Count[Nothing] = 0
}
