package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{Count as _, *}


opaque type Id >: Int = Int
opaque type SlotId >: Int <: Id = Int
opaque type TopicId >: Int <: Id = Int
opaque type PersonId >: Int <: Id = Int

object Id {
  extension (id: Id) {
    inline def value: Int = id
  }

  inline def None: Id = -1
}

object SlotId {
  inline def None: SlotId = -1
}

object TopicId {
  inline def None: TopicId = -1

  inline def Absent: TopicId = 0 // Topic for someone who isn't there
}

object PersonId {
  inline def None: PersonId = -1
}

opaque type Count[+I <: Id] >: Int = Int

object Count {
  extension [I >: Int <: Id](c: Count[I]) {
    inline def value: Int = c

    inline def foreach(inline f: I => Unit) = fastLoop(0, c)(f)

    inline def foreachTo(limit: I)(inline f: I => Unit) = fastLoop(0, limit + 1)(f)

    inline def foreachUntil(limit: I)(inline f: I => Unit) = fastLoop(0, limit)(f)

    // TODO inline this method
    /** Returns the first id in that count matching the condition. If none matches, returns Id.None. */
    def find(f: I => Boolean): I = {
      var i: I = 0
      var notFound = true
      var result: I = Id.None
      while (notFound && i < c) {
        if (f(i)) {
          result = i
          notFound = false
        }
        i += 1
      }
      result
    }

    /* TODO flatIndexes should be in the appropriate classes */
    inline def flatIndex(inline h: (Id | Int), i: I) = h * c + i
  }

  /** A maximum count that won't overflow when it's summed with others */
  inline def maxCount[I >: Int <: Id]: Count[I] = 10_000

  val Zero: Count[Nothing] = 0
}
