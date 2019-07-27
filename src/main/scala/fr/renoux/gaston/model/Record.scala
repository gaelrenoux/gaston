package fr.renoux.gaston.model


/** A Record is a triplet of slot, topic and assigned persons */
case class Record(slot: Slot, topic: Topic, persons: Set[Person]) extends Ordered[Record] {

  lazy val personsSeq: Seq[Person] = persons.toSeq

  /* No need to compare persons, on a given schedule there is not two records with the same slot and topic */
  override def compare(that: Record): Int = {
    val compareSlots = slot.name.compareTo(that.slot.name)
    if (compareSlots != 0) compareSlots
    else topic.name.compareTo(that.topic.name)
  }

  lazy val optionalPersons: Set[Person] = persons -- topic.mandatory

  lazy val canRemovePersons: Boolean = persons.size > topic.min && optionalPersons.nonEmpty
  lazy val canAddPersons: Boolean = persons.size < topic.max
}

object Record {
  def fromTuple(tuple: (Slot, Topic, Set[Person])): Record = Record(tuple._1, tuple._2, tuple._3)

  def fromTuple2(tuple: ((Slot, Topic), Set[Person])): Record = Record(tuple._1._1, tuple._1._2, tuple._2)

  def apply(slot: Slot, topic: Topic, persons: Person*): Record = apply(slot, topic, persons.toSet)
}
