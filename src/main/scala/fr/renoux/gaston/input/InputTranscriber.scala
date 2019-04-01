package fr.renoux.gaston.input

import fr.renoux.gaston.model._
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference}
import fr.renoux.gaston.model.problem.ProblemImpl
import scalaz.Validation
import scalaz.syntax.validation._

/** Converts the Input object to the Problem object. */
object InputTranscriber {

  /** What score should a person have if all its preferences are satisfied ? */
  private val personTotalScore: Double = 1000.0

  /** Load the real input from the user model. */
  def transcribe(inputRoot: InputRoot): Validation[InputErrors, Problem] = {
    //TODO better validation !
    val input: InputModel = inputRoot.gaston

    val slotsPerName = input.slots.map { s => s.name -> Slot(s.name) }.toMap
    val topicsPerName = input.topics.map { t => t.name -> Topic(t.name) }.toMap
    val personsPerName = input.persons.map { p => p.name -> Person(p.name, p.weight) }.toMap
    val ctx = Context(slotsPerName, topicsPerName, personsPerName)

    val constraints = Set[Constraint]() ++
      getSlotMaxesConstraints(input, ctx) ++
      getAbsenceConstraints(input, ctx) ++
      getInterdictionConstraints(input, ctx) ++
      getObligationConstraints(input, ctx) ++
      getNumberConstraints(input, ctx) ++
      getForcedTopicConstraints(input, ctx) ++
      getSimultaneousTopicsConstraints(input, ctx) ++
      getExclusiveTopicsConstraints(input, ctx)

    val preferences = Set[Preference]() ++
      getGroupAntiPreferences(input, ctx) ++
      getPersonTopicPreferences(input, ctx)

    new ProblemImpl(
      slotsPerName.values.toSet,
      topicsPerName.values.toSet,
      personsPerName.values.toSet,
      constraints,
      preferences
    ).success
  }

  private def getSlotMaxesConstraints(input: InputModel, ctx: Context): Set[SlotMaxTopicCount] =
    input.slots.flatMap { inSlot =>
      val slot = ctx.slotsPerName(inSlot.name)
      val maxOption = inSlot.maxTopics orElse input.settings.defaultMaxTopicsPerSlot
      maxOption.map(SlotMaxTopicCount(slot, _))
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
      val min = inTopic.min.getOrElse(input.settings.defaultMinPersonsPerTopic)
      val max = inTopic.max.getOrElse(input.settings.defaultMaxPersonsPerTopic)
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
