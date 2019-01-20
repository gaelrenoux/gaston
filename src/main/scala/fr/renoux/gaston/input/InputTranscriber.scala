package fr.renoux.gaston.input

import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference, Preference}
import fr.renoux.gaston.model.problem.Problem
import fr.renoux.gaston.model.{Person, Slot, Topic, Weight}
import scalaz.ValidationNel
import scalaz.syntax.validation._

/** Converts the Input object to the Problem object. */
object InputTranscriber {

  /** Load the real input from the user model. */
  def transcribe(inputRoot: InputRoot): ValidationNel[String, Problem] = {
    //TODO better validation !
    val input: InputModel = inputRoot.gaston

    val slotsPerName = input.slots.map { s => s -> Slot(s) }.toMap
    val topicsPerName = input.topics.map { t => t.name -> Topic(t.name) }.toMap
    val personsPerName = input.persons.map { p => p.name -> Person(p.name, Weight(p.weight)) }.toMap
    val ctx = Context(slotsPerName, topicsPerName, personsPerName)

    val absenceConstraints = getAbsenceConstraints(input, ctx)

    val interdictionConstraints = getInterdictionConstraints(input, ctx)

    val obligationConstraints = getObligationConstraints(input, ctx)

    val numberConstraints = getNumberConstraints(input, ctx)

    val forcedSlotConstraints = getForcedTopicConstraints(input, ctx)

    val simultaneousTopicsConstraints = getSimultaneousTopicsConstraints(input, ctx)

    val exclusiveTopicsConstraints = getExclusiveTopicsConstraints(input, ctx)

    val constraints = Set[Constraint]() ++ absenceConstraints ++ interdictionConstraints ++ obligationConstraints ++
      numberConstraints ++ forcedSlotConstraints ++ simultaneousTopicsConstraints ++ exclusiveTopicsConstraints

    val incompatibilityPreferences = getGroupAntiPreferences(input, ctx)

    val personPreferences = getTopicPreferences(input, ctx)

    val preferences = Set[Preference]() ++ incompatibilityPreferences ++ personPreferences

    Problem(
      slotsPerName.values.toSet,
      topicsPerName.values.toSet,
      personsPerName.values.toSet,
      constraints,
      preferences
    ).success
  }

  private def getAbsenceConstraints(input: InputModel, ctx: Context): Set[PersonAbsence] =
    input.persons.flatMap { ip =>
      ip.absences.map(ctx.slotsPerName).map(PersonAbsence(ctx.personsPerName(ip.name), _))
    }

  private def getInterdictionConstraints(input: InputModel, ctx: Context): Set[PersonTopicInterdiction] =
    input.topics.flatMap { it =>
      it.forbidden.map(ctx.personsPerName).map(PersonTopicInterdiction(_, ctx.topicsPerName(it.name)))
    }

  private def getObligationConstraints(input: InputModel, ctx: Context): Set[PersonTopicObligation] =
    input.topics.flatMap { it =>
      it.mandatory.map(ctx.personsPerName).map(PersonTopicObligation(_, ctx.topicsPerName(it.name)))
    }


  private def getNumberConstraints(input: InputModel, ctx: Context): Set[TopicNeedsNumberOfPersons] =
    input.topics.map { inTopic =>
      val topic = ctx.topicsPerName(inTopic.name)
      val min = inTopic.min.getOrElse(input.settings.defaultMin)
      val max = inTopic.max.getOrElse(input.settings.defaultMax)
      TopicNeedsNumberOfPersons(topic, min, max)
    }

  private def getForcedTopicConstraints(input: InputModel, maps: Context): Set[TopicForcedSlot] =
    input.topics.flatMap { inTopic =>
      inTopic.forcedSlot.map(maps.slotsPerName).map(TopicForcedSlot(maps.topicsPerName(inTopic.name), _))
    }

  private def getSimultaneousTopicsConstraints(input: InputModel, ctx: Context): Set[TopicsSimultaneous] =
    input.constraints.map(_.simultaneous).getOrElse(Set()).map { inConstraint =>
      TopicsSimultaneous(inConstraint.topics.map(ctx.topicsPerName))
    }

  private def getExclusiveTopicsConstraints(input: InputModel, ctx: Context): Set[TopicsExclusive] =
    input.constraints.map(_.exclusive).getOrElse(Set()).map { inConstraint =>
      TopicsExclusive(inConstraint.topics.map(ctx.topicsPerName), inConstraint.exemptions.map(ctx.personsPerName))
    }

  private def getGroupAntiPreferences(input: InputModel, ctx: Context): Set[PersonGroupAntiPreference] =
    input.persons.collect {
      case ip if ip.incompatible.nonEmpty =>
        val person = ctx.personsPerName(ip.name)
        val group = ip.incompatible.map(ctx.personsPerName)
        PersonGroupAntiPreference(person, group, input.settings.incompatibilityAntiPreference)
    }

  private def getTopicPreferences(input: InputModel, ctx: Context): Set[PersonTopicPreference] = {
    input.preferences.flatMap { case InputPreference(p, strongs, weaks) =>
      val person = ctx.personsPerName(p)
      val all = strongs.map((_, input.settings.strongPreference)) ++ weaks.map((_, input.settings.weakPreference))
      all.map { case (t, reward) =>
        PersonTopicPreference(person, ctx.topicsPerName(t), reward)
      }
    }
  }

  private case class Context(
      slotsPerName: Map[String, Slot],
      topicsPerName: Map[String, Topic],
      personsPerName: Map[String, Person]
  )

}
