package fr.renoux.gaston.model.problem

import fr.renoux.gaston.model.constraints.{Constraint, PersonAbsence, PersonTopicObligation, TopicNeedsNumberOfPersons}
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
                    preferences: Set[Preference]
                  ) {

  //TODO deduplicate constraints (multipe prefs with the same person and topic, for instance)

  lazy val mandatoryPersonsPerTopic = {

    val fromConstraints = constraints collect {
      case PersonTopicObligation(person, topic) => topic -> person
    } groupBy (_._1) mapValues (_ map (_._2))

    val topicsWithNoMandatoryPerson = (topics filterNot fromConstraints.keySet) map (_ -> Set[Person]()) toMap

    fromConstraints ++ topicsWithNoMandatoryPerson
  }

  lazy val personSlotsPossibilities = {
    val notedAbsences = constraints collect {
      case PersonAbsence(p, s) => (p, s)
    }
    for {
      p <- persons
      s <- slots
      if !notedAbsences((p, s))
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

  /** how many topics can we have during the same slot */
  lazy val parallelization = (topics.size.toDouble / slots.size).ceil.toInt

  lazy val minNumberPerTopic = constraints.collect {
    case TopicNeedsNumberOfPersons(t, min, max) if min.isDefined => (t, min.get)
  } toMap

  lazy val maxNumberPerTopic = constraints.collect {
    case TopicNeedsNumberOfPersons(t, min, max) if max.isDefined => (t, max.get)
  } toMap

}

