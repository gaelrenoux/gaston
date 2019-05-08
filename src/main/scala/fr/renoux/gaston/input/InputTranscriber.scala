package fr.renoux.gaston.input

import fr.renoux.gaston.model._
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference}
import fr.renoux.gaston.model.problem.ProblemImpl
import scalaz.Validation
import scalaz.syntax.validation._

/** Converts the Input object to the Problem object. */
object InputTranscriber {

  private val EmptyTopicNegativeScore = Score.PersonTotalScore.negative  * 0.02

  /** Load the real input from the user model. */
  def transcribe(inputRoot: InputRoot): Validation[InputErrors, Problem] = {
    //TODO better validation !
    val input: InputModel = inputRoot.gaston

    val slotSequences = input.slots.map(_.map(s => Slot(s.name)))
    val slotSet = slotSequences.flatten.toSet
    val slotsPerName = slotSet.map(s => s.name -> s).toMap
    val topicsPerName = input.topics.map { t => t.name -> Topic(t.name) }.toMap
    val personsPerName = input.persons.map { p => p.name -> Person(p.name, p.weight) }.toMap
    val ctx = Context(slotsPerName, topicsPerName, personsPerName)

    val (nothingTopics, nothingTopicsConstraints, nothingTopicsPreferences) =
      if (input.settings.maxPersonsOnNothing <= 0 || input.settings.maxPersonsOnNothing < input.settings.minPersonsOnNothing) {
        (Set.empty[Topic], Set.empty[Constraint], Set.empty[Preference])

      } else {
        val nothings = slotsPerName.values.map { s =>
          val topic = Topic.nothing(s)
          val countConstraint = TopicNeedsNumberOfPersons(topic, input.settings.minPersonsOnNothing, input.settings.maxPersonsOnNothing)
          val slotConstraint = TopicForcedSlot(topic, Set(s))
          val antiPreferences = personsPerName.values.map(PersonTopicPreference(_, topic, EmptyTopicNegativeScore))
          (topic, countConstraint, slotConstraint, antiPreferences)
        }

        val topics = nothings.map(_._1)
        val constraints = nothings.map(_._2) ++ nothings.map(_._3)
        val preferences = nothings.flatMap(_._4)
        (topics, constraints, preferences)
      }

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
      slotSequences,
      topicsPerName.values.toSet ++ nothingTopics.toSet,
      personsPerName.values.toSet,
      constraints ++ nothingTopicsConstraints.toSet,
      preferences ++ nothingTopicsPreferences.toSet
    ).success
  }

  private def getSlotMaxesConstraints(input: InputModel, ctx: Context): Set[SlotMaxTopicCount] =
    input.slots.flatten.toSet[InputSlot].flatMap { inSlot =>
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
    for {
      inTopic <- input.topics
      topic = ctx.topicsPerName(inTopic.name)
      min = inTopic.min.getOrElse(input.settings.defaultMinPersonsPerTopic)
      max = inTopic.max.getOrElse(input.settings.defaultMaxPersonsPerTopic)
    } yield TopicNeedsNumberOfPersons(topic, min, max)

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
      totalInputScore = inPerson.wishes.values.sum.value
      scoreFactor = Score.PersonTotalScore.value / totalInputScore
      inWish <- inPerson.wishes
      topic = ctx.topicsPerName(inWish._1)
    } yield PersonTopicPreference(person, topic, inWish._2 * scoreFactor)


  private case class Context(
      slotsPerName: Map[String, Slot],
      topicsPerName: Map[String, Topic],
      personsPerName: Map[String, Person]
  )

}
