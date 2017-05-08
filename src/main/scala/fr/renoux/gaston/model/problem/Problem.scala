package fr.renoux.gaston.model.problem

import fr.renoux.gaston.model.constraints.{Constraint, PersonPresence, PersonTopicObligation}
import fr.renoux.gaston.model.preferences.Preference
import fr.renoux.gaston.model.{Person, Slot, Topic}
import fr.renoux.gaston.util.CollectionImplicits._

/**
  * Created by gael on 07/05/17.
  */
case class Problem(
                    slots: Set[Slot],
                    topics: Set[Topic],
                    persons: Set[Person],
                    constraints: Set[Constraint],
                    preferences: Set[Preference],
                    defaultPersonPresence: Boolean = true
                  ) {

  //TODO deduplicate constraints (multipe prefs with the same person and topic, for instance)

  lazy val mandatoryPersonsPerTopic = constraints collect {
    case PersonTopicObligation(person, topic) => topic -> person
  } groupBy (_._1) mapValues (_ map (_._2))

  lazy val personSlotsPossibilities = {
    val noted = constraints collect {
      case PersonPresence(p, s, bool) if bool != defaultPersonPresence => (p, s)
    }
    for {
      p <- persons
      s <- slots
      if noted((p, s)) != defaultPersonPresence
    } yield (p, s)
  }

  lazy val slotsPerPerson = personSlotsPossibilities.groupToMap

  lazy val personsPerSlot = personSlotsPossibilities.map(_.swap).groupToMap

  lazy val incompatibleTopicsPerTopic = {
    val couples = for {
      topic1 <- topics
      topic2 <- topics
      if mandatoryPersonsPerTopic(topic1).intersect(mandatoryPersonsPerTopic(topic2)).nonEmpty
    } yield (topic1, topic2)

    couples.groupToMap
  }

  lazy val incompatibleTopicsPerSlot = {
    val couples = for {
      slot <- slots
      topic <- topics
      if personsPerSlot(slot).intersect(mandatoryPersonsPerTopic(topic)).isEmpty
    } yield (slot, topic)
    couples.groupToMap
  }


}

