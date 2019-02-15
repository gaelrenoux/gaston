package fr.renoux.gaston.input

import fr.renoux.gaston.model._
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference}
import fr.renoux.gaston.model.problem.ProblemImpl
import scalaz.ValidationNel
import scalaz.syntax.validation._

/** Converts the Input object to the Problem object. */
object InputTranscriber {

  /** What score should a person have if all its preferences are satisfied ? */
  private val personTotalScore: Double = 1000.0

  /** Load the real input from the user model. */
  def transcribe(inputRoot: InputRoot): ValidationNel[String, Problem] = {
    //TODO better validation !
    val input: InputModel = inputRoot.gaston

    val slotsPerName = input.slots.map { s => s -> Slot(s) }.toMap
    val topicsPerName = input.topics.map { t => t.name -> Topic(t.name) }.toMap
    val personsPerName = input.persons.map { p => p.name -> Person(p.name, p.weight) }.toMap
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

    val personTopicPreferences = getPersonTopicPreferences(input, ctx)

    val preferences = Set[Preference]() ++ incompatibilityPreferences ++ personTopicPreferences

    val parallelization = input.settings.parallelization.getOrElse(
      (topicsPerName.size.toDouble / slotsPerName.size).ceil.toInt)

    new ProblemImpl(
      parallelization,
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
    for {
      ip <- input.persons
      person = ctx.personsPerName(ip.name)
      topicName <- ip.forbidden
      topic = ctx.topicsPerName(topicName)
    } yield PersonTopicInterdiction(person, topic)


  private def getObligationConstraints(input: InputModel, ctx: Context): Set[PersonTopicObligation] =
    for {
      ip <- input.persons
      person = ctx.personsPerName(ip.name)
      topicName <- ip.mandatory
      topic = ctx.topicsPerName(topicName)
    } yield PersonTopicObligation(person, topic)

  private def getNumberConstraints(input: InputModel, ctx: Context): Set[TopicNeedsNumberOfPersons] =
    input.topics.map { inTopic =>
      val topic = ctx.topicsPerName(inTopic.name)
      val min = inTopic.min.getOrElse(input.settings.defaultMin)
      val max = inTopic.max.getOrElse(input.settings.defaultMax)
      TopicNeedsNumberOfPersons(topic, min, max)
    }

  private def getForcedTopicConstraints(input: InputModel, maps: Context): Set[TopicForcedSlot] =
    input.topics.flatMap { inTopic =>
      inTopic.slots.map { slots =>
        val ss = slots.map(maps.slotsPerName)
        TopicForcedSlot(maps.topicsPerName(inTopic.name), ss)
      }
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

  /** Person wishes are scaled so that everyone has the same maximum score. This avoids the problem where someone puts
    * few preferences or with low value only, where he would always stay "unhappy" and therefore privileged when
    * improving the schedule. */
  private def getPersonTopicPreferences(input: InputModel, ctx: Context): Set[PersonTopicPreference] =
    for {
      inPerson <- input.persons
      person = ctx.personsPerName(inPerson.name)
      totalInputScore = inPerson.wishes.map(iw => iw.value.value * iw.topics.size).sum
      scoreFactor = personTotalScore / totalInputScore
      inWishes <- inPerson.wishes
      topicName <- inWishes.topics
      topic = ctx.topicsPerName(topicName)
    } yield PersonTopicPreference(person, topic, inWishes.value * scoreFactor)


  private case class Context(
      slotsPerName: Map[String, Slot],
      topicsPerName: Map[String, Topic],
      personsPerName: Map[String, Person]
  )

}
