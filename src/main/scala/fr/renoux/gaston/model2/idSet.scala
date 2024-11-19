package fr.renoux.gaston.model2

import scala.collection.immutable.BitSet


/** IdSet: a set of arbitrary Ids as an actual BitSet. */
opaque type IdSet[I <: Id] = BitSet

object IdSet {
  extension [I <: Id](s: IdSet[I]) {
    inline def apply(id: I): Boolean = s(id.value)
  }
}
