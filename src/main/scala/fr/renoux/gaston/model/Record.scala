package fr.renoux.gaston.model


/** A Record is a triplet of slot, topic and assigned persons */
case class Record(slot: Slot, topic: Topic, persons: Set[Person])

object Record {
  def fromTuple(tuple: (Slot, Topic, Set[Person])) = Record(tuple._1, tuple._2, tuple._3)

  def fromTuple2(tuple: ((Slot, Topic), Set[Person])) = Record(tuple._1._1, tuple._1._2, tuple._2)

  def apply(slot: Slot, topic: Topic, persons: Person*): Record = apply(slot, topic, persons.toSet)
}