package fr.renoux.gaston.model.preferences

import fr.renoux.gaston.model.*
import fr.renoux.gaston.util.ArraySet

/**
 * That one person doesn't want to share a schedule record with any person from that group. The reward here is
 * negative, as we count the number of such occurrences. Note that it does not apply on the members of the group, who
 * may have no anti-preference towards the person.
 */
final case class PersonGroupAntiPreference(
    person: Person,
    group: ArraySet[Person],
    reward: Score
) extends Preference.RecordLevel with Preference.Anti with Preference.Personal {

  override def scoreRecord(record: Record): Score = {
    if (record.persons.contains(person)) {
      val count = record.persons.count(group.contains)
      if (count == 0) Score.Zero else reward * count
    } else Score.Zero
  }

  override def equals(o: Any): Boolean = o match {
    case that: PersonGroupAntiPreference => this.person == that.person && this.group.actualEquals(that.group) && this.reward == that.reward
    case _ => false
  }

  override def hashCode(): Int = (this.person, this.group.actualHashCode, reward).hashCode()

  override lazy val toLongString: String = s"PersonGroupAntiPreference(${person.toShortString}, $group, $reward)"

  override lazy val toAbstract: (String, Person.Id, Seq[Person.Id], Double) = ("PersonGroupAntiPreference", person.id, group.toIdSet.toSeq.sorted, reward.value)
}
