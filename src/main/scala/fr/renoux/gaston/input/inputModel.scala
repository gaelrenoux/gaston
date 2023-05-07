package fr.renoux.gaston.input

import eu.timepit.refined._
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric._
import eu.timepit.refined.types.string.NonEmptyString
import fr.renoux.gaston.input.InputRefinements._
import fr.renoux.gaston.model.{Score, Topic, Weight}
import fr.renoux.gaston.util.Opt

/* All line and column indices are zero-based */
// scalastyle:off magic.number

case class InputModel(
    settings: InputSettings = InputSettings(),
    tableSettings: InputTableSettings = InputTableSettings(),
    slots: List[List[InputSlot]] = Nil,
    persons: List[InputPerson] = Nil,
    topics: List[InputTopic] = Nil,
    constraints: InputGlobalConstraints = InputGlobalConstraints()
) {
  lazy val slotsSet: Set[InputSlot] = slots.flatten.toSet
  lazy val topicsSet: Set[InputTopic] = topics.toSet
  lazy val personsSet: Set[InputPerson] = persons.toSet
  lazy val slotsNameSet: Set[NonEmptyString] = slots.flatten.map(_.name).toSet
  lazy val topicsNameSet: Set[NonEmptyString] = topics.map(_.name).toSet
  lazy val personsNameSet: Set[NonEmptyString] = persons.map(_.name).toSet
  lazy val multipleTopicsSet: Set[InputTopic] = topicsSet.filter(_.multiple.exists(_ > 1))
}

case class InputSettings(
    incompatibilityAntiPreference: NonPosScore = NonPosScore(-1000.0),
    defaultMaxTopicsPerSlot: Option[PosInt] = None,
    defaultMinPersonsPerTopic: PosInt = PosInt.unsafeFrom(Topic.DefaultMin),
    defaultMaxPersonsPerTopic: PosInt = PosInt.unsafeFrom(Topic.DefaultMax),
    maxPersonsOnNothing: NonNegInt = 0,
    minPersonsOnNothing: NonNegInt = 0,
    personOnNothingAntiPreference: NonPosScore = NonPosScore(-100.0),
    personOnNothingAntiPreferenceScaling: Option[InputSettings.PersonOnNothingAntiPreferenceScaling] = None,
    backtrackInitialSchedule: Boolean = true // TODO Should be calculated
) {
  lazy val isNothingEnabled: Boolean = maxPersonsOnNothing > 0
}

object InputSettings {
  /** If this is enabled, the anti-preference for nothing will scale with the number of forbidden topics for each person.
    * @param forbiddenRatioForMaximum At this ratio of forbidden topics, the anti-preference will be up to its maximal (negative) value
    */
  case class PersonOnNothingAntiPreferenceScaling(
      enabled: Boolean = true,
      forbiddenRatioForMaximum: Double = 0.75,
      maximumAntiPreference: NonPosScore = NonPosScore(-1.0),
  )
}

case class InputTableSettings(
    separator: NonEmptyString = "\t",
    personsRow: NonNegInt = 0,
    wishesStartRow: NonNegInt = 1,
    personsStartCol: NonNegInt = 4,
    topicCol: NonNegInt = 0,
    topicOccurrencesCol: Option[NonNegInt] = None,
    mandatoryPersonCol: NonNegInt = 1,
    minPersonsCol: Option[NonNegInt] = None,
    maxPersonsCol: NonNegInt = 3,
    personsCountAdd: NonNegInt = 0,
    mandatoryPersonWeight: PosWeight = DefaultWeightRefined,
    forbiddenPersonMarker: Option[String] = None,
    preferencesScoreMapping: Option[Map[String, Score]] = None
)

case class InputSlot(
    name: NonEmptyString,
    maxTopics: Option[PosInt] = None
)

case class InputTopic(
    name: NonEmptyString,
    min: Option[PosInt] = None,
    max: Option[PosInt] = None,
    occurrences: Option[PosInt] = None,
    multiple: Option[PosInt] = None, // multi-topic: on the same slot, occupies several groups of persons
    slots: Option[Set[NonEmptyString]] = None,
    presence: Option[Score] = None,
    forced: Boolean = false
) {
  /** Occurrence needs to be an Option to not appear when not needed */
  lazy val forcedOccurrences: PosInt = occurrences.getOrElse(1: PosInt)

  /** Multiple needs to be an Option to not appear when not needed */
  lazy val forcedMultiple: PosInt = multiple.getOrElse(1: PosInt)

  /** Duplicate this Topic by its number of occurrences */
  lazy val occurrenceInstances: Seq[InputTopic.Occurrence] =
    if (forcedOccurrences.value == 1) Seq(InputTopic.Occurrence(this)) else {
      (1 to forcedOccurrences).map(InputTopic.Occurrence(this, _))
    }
}

object InputTopic {
  val OccurrenceMarker = "#"

  val MultipleMarker = "~"

  /** For multi-occurrence topic: the same topic will appear on different slots */
  case class Occurrence(
      inputTopic: InputTopic,
      index: Opt[Int] = Opt.Missing // present only if there are multiple occurrences
  ) {
    lazy val name: String =
      index.fold(inputTopic.name.value)(i => s"${inputTopic.name} $OccurrenceMarker$i")

    /* Duplicate topics if topic is multiple. Must be after the occurrence, so that he multiple marker appears after the occurrence one. */
    lazy val multipleParts: Seq[InputTopic.Part] =
      if (inputTopic.forcedMultiple.value == 1) Seq(Part(this))
      else (1 to inputTopic.forcedMultiple).map(Part(this, _))
  }

  /** For multiple topics: each part of the topic, which will occupy a group of persons each */
  case class Part(
      occurrence: Occurrence,
      index: Opt[Int] = Opt.Missing // present only if the topic has a multiplicity > 1
  ) {
    lazy val name: String =
      index.fold(occurrence.name)(i => s"${occurrence.name} $MultipleMarker$i")
  }

}

case class InputPerson(
    name: NonEmptyString,
    weight: PosWeight = DefaultWeightRefined,
    absences: Set[NonEmptyString] = Set.empty,
    mandatory: Set[NonEmptyString] = Set.empty,
    forbidden: Set[NonEmptyString] = Set.empty,
    incompatible: Set[NonEmptyString] = Set.empty,
    wishes: Map[String, Score] = Map.empty // can't use Refined as a key, see https://github.com/fthomas/refined/issues/443
)

case class InputGlobalConstraints(
    simultaneous: Set[InputSimultaneousConstraint] = Set.empty,
    notSimultaneous: Set[InputSimultaneousConstraint] = Set.empty,
    exclusive: Set[InputExclusiveConstraint] = Set.empty,
    linked: Set[InputLinkedConstraint] = Set.empty,
)

case class InputSimultaneousConstraint(
    topics: Set[NonEmptyString]
)

case class InputExclusiveConstraint(
    topics: Set[NonEmptyString],
    exemptions: Set[NonEmptyString] = Set.empty
)

case class InputLinkedConstraint(
    topics: Set[NonEmptyString]
)

object InputRefinements {

  class ScoreNonPositive()

  implicit val scoreNonPositiveValidate: Validate.Plain[Score, ScoreNonPositive] =
    Validate.fromPredicate(s => s.value <= 0, s => s"($s is negative or zero)", new ScoreNonPositive())

  type NonPosScore = Score Refined ScoreNonPositive

  object NonPosScore {
    def apply(s: NonPosDouble): NonPosScore = refineV[ScoreNonPositive](Score(s)).getOrElse(throw new IllegalArgumentException(s.toString))
  }

  class WeightPositive()

  implicit val weightPositiveValidate: Validate.Plain[Weight, WeightPositive] =
    Validate.fromPredicate(w => w.value > 0, w => s"($w is positive)", new WeightPositive())

  type PosWeight = Weight Refined WeightPositive

  object PosWeight {
    def apply(w: PosDouble): PosWeight = refineV[WeightPositive](Weight(w)).getOrElse(throw new IllegalArgumentException(w.toString))
  }

}

// scalastyle:on magic.number
