package fr.renoux.gaston.input

import fr.renoux.gaston.model._
import fr.renoux.gaston.model.constraints._
import fr.renoux.gaston.model.preferences.{PersonGroupAntiPreference, PersonTopicPreference, TopicsExclusive}
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
    val topicsPerName = input.topics.map { inTopic =>
      inTopic.forcedOccurrences match {
        case 1 => inTopic.name -> Set(Topic(inTopic.name))
        case c if c > 0 => inTopic.name -> (1 to c).toSet[Int].map(i => Topic(s"${inTopic.name} $i"))
        case _ => throw new IllegalArgumentException("Can't have a negative number of occurrences") //TODO use Refined instead
      }
    }.toMap
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
      getForcedSlotConstraints(input, ctx) ++
      getObligationConstraints(input, ctx) ++
      getNumberConstraints(input, ctx) ++
      getSimultaneousTopicsConstraints(input, ctx)

    val preferences = Set[Preference]() ++
      getGroupAntiPreferences(input, ctx) ++
      getPersonTopicPreferences(input, ctx) ++
      getInterdictionPreferences(input, ctx) ++
      getExclusiveTopicsPreferences(input, ctx) ++
      getExclusiveOccurrencesPreferences(input, ctx)

    new ProblemImpl(
      slotSequences,
      topicsPerName.values.flatten.toSet ++ nothingTopics.toSet,
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

  private def getForcedSlotConstraints(input: InputModel, ctx: Context) =
    input.topics.flatMap {
      case inTopic if inTopic.slots.nonEmpty =>
        val topics = ctx.topicsPerName(inTopic.name)
        val slots = inTopic.slots.get.map(ctx.slotsPerName)
        topics.map(TopicForcedSlot(_, slots))
      case _ => Set.empty[TopicForcedSlot]
    }

  private def getInterdictionPreferences(input: InputModel, ctx: Context): Set[PersonTopicPreference] =
    for {
      ip <- input.persons
      person = ctx.personsPerName(ip.name)
      topicName <- ip.forbidden
      topic <- ctx.topicsPerName(topicName)
    } yield PersonTopicPreference(person, topic, Score.PersonTotalScore.negative)

  private def getObligationConstraints(input: InputModel, ctx: Context): Set[PersonTopicObligation] =
    for {
      ip <- input.persons
      person = ctx.personsPerName(ip.name)
      topicName <- ip.mandatory
      topic <- ctx.topicsPerName(topicName)
    } yield PersonTopicObligation(person, topic)

  private def getNumberConstraints(input: InputModel, ctx: Context): Set[TopicNeedsNumberOfPersons] =
    for {
      inTopic <- input.topics
      topic <- ctx.topicsPerName(inTopic.name)
      min = inTopic.min.getOrElse(input.settings.defaultMinPersonsPerTopic)
      max = inTopic.max.getOrElse(input.settings.defaultMaxPersonsPerTopic)
    } yield TopicNeedsNumberOfPersons(topic, min, max)

  private def getSimultaneousTopicsConstraints(input: InputModel, ctx: Context): Set[TopicsSimultaneous] =
    input.constraints.map(_.simultaneous).getOrElse(Set()).map { inConstraint =>
      //TODO Better handling of simultaneous and multiple, should at least return an error
      TopicsSimultaneous(inConstraint.topics.map(ctx.topicsPerName(_).head))
    }

  private def getExclusiveTopicsPreferences(input: InputModel, ctx: Context): Set[TopicsExclusive] =
    input.constraints.map(_.exclusive).getOrElse(Set()).map { inConstraint =>
      TopicsExclusive(inConstraint.topics.flatMap(ctx.topicsPerName), inConstraint.exemptions.map(ctx.personsPerName), Score.PersonTotalScore.negative * 100)
    }

  private def getExclusiveOccurrencesPreferences(input: InputModel, ctx: Context): Set[TopicsExclusive] =
    input.topics.filter(_.occurrences.exists(_ > 1)).map { inTopic =>
      val topics = ctx.topicsPerName(inTopic.name)
      val mandatories = input.persons.filter(_.mandatory.contains(inTopic.name)).map(ip => ctx.personsPerName(ip.name))
      TopicsExclusive(topics, mandatories, Score.PersonTotalScore.negative * 100)
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
      topic <- ctx.topicsPerName(inWish._1)
    } yield PersonTopicPreference(person, topic, inWish._2 * scoreFactor)


  private case class Context(
      slotsPerName: Map[String, Slot],
      topicsPerName: Map[String, Set[Topic]],
      personsPerName: Map[String, Person]
  )

}
