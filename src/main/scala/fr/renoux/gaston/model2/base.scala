package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{Count as _, *}


// TODO inline all closures

/* ********************* All of the IDs ********************* */

opaque type Id >: Int = Int
opaque type SlotId >: Int <: Id = Int
opaque type TopicId >: Int <: Id = Int
opaque type PersonId >: Int <: Id = Int

object Id {
  inline def None: Id = -1

  extension (id: Id) {
    inline def value: Int = id
  }
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

opaque type Count[I <: Id] >: Int = Int

object Count {
  extension [I >: Int <: Id](c: Count[I]) {
    inline def value: Int = c

    inline def foreach(inline f: I => Unit) = fastLoop(0, c)(f)

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

    inline def flatIndex(inline h: (Id | Int), i: I) = h * c + i
  }
}
