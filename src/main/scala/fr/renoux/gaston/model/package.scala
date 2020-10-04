package fr.renoux.gaston

package object model {

  def slotBitSet(it: Iterable[Slot])(implicit counts: Counts): BitSet[Slot] =
    BitSet.from[Slot](counts.slots)(it)

  def topicBitSet(it: Iterable[Topic])(implicit counts: Counts): BitSet[Topic] =
    BitSet.from[Topic](counts.topics)(it)

  def personBitSet(it: Iterable[Person])(implicit counts: Counts): BitSet[Person] =
    BitSet.from[Person](counts.persons)(it)

}
