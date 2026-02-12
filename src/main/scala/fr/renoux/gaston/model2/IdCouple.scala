package fr.renoux.gaston.model2


/** IdCouple: two arbitrary ids, encoded as a Long. */
opaque type IdCouple[I <: Id, J <: Id] = Long

object IdCouple {
  inline def apply[I <: Id, J <: Id](a: I, b: J): IdCouple[I, J] = {
    (a.value.toLong << 32) | (b.value.toLong & 0xffffffffL)
  }

  extension [I <: Id, J <: Id](ic: IdCouple[I, J]) {
    inline def _1: I = (ic >> 32).toInt.asInstanceOf[I] // enforce type
    inline def _2: J = ic.toInt.asInstanceOf[J] // enforce type
  }
}
