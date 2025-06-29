package fr.renoux.gaston.input

import fr.renoux.gaston.model.{Score, Weight}
import fr.renoux.gaston.util.*
import pureconfig.*
import pureconfig.generic.*
import pureconfig.generic.semiauto.deriveConvert
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.pureconfig.given

import scala.concurrent.duration.{DurationInt, FiniteDuration}

// TODO Drop all unsafeFrom calls once https://github.com/fthomas/refined/issues/932 is solved

type NonEmptyString = String :| Not[Empty]
type PosInt = Int :| Positive
type NonNegInt = Int :| Positive0
type PosDouble = Double :| Positive

type NonPosScore = Score :| Negative0

def NonPosScore(i: Double :| Negative0): NonPosScore = Score(i).asInstanceOf[NonPosScore] // TODO Ugly, but changing Scores soon

given Constraint[Score, Positive] with {
  override inline def test(inline value: Score): Boolean = value.value > 0

  override inline def message: String = "Should be strictly positive"
}

given Constraint[Score, Positive0] with {
  override inline def test(inline value: Score): Boolean = value.value >= 0

  override inline def message: String = "Should be positive or zero"
}

given Constraint[Score, Negative] with {
  override inline def test(inline value: Score): Boolean = value.value < 0

  override inline def message: String = "Should be strictly negative"
}

given Constraint[Score, Negative0] with {
  override inline def test(inline value: Score): Boolean = value.value <= 0

  override inline def message: String = "Should be negative or zero"
}

/* All line and column indices are zero-based */

/* These classes defined the model as it is represented in the canonical input (.conf files). It is made to be as
user-friendly as possible in the configuration file, and in second to be comfortable to use by the developer. However,
it is not optimized for performance.
*/

// TODO Shouldn't be used outside of the input package
object Constants {

  val DefaultTopicMin: PosInt = 1

  val DefaultTopicMax: PosInt = 10

  /** What score should a person have if all its preferences are satisfied ? */
  val PersonTotalScore: Score = Score(1000.0)

  /** A low "max-value", so that we can still add it and not overflow, but which can still be used as a max value when we want to max. */
  // TODO that's horrible, remove this. But it helps for now.
  val IntLowMaxValue: PosInt = 10000

}

final case class InputModel(
    settings: InputSettings = InputSettings(),
    tableSettings: InputTableSettings = InputTableSettings(),
    slots: List[List[InputSlot]] = List(Nil), // Presents better than Nil when generating an input
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
  lazy val topicsByName: Map[NonEmptyString, InputTopic] = topics.view.zipWith(_.name).map(_.swap).toMap
}

final case class InputSettings(
    incompatibilityAntiPreference: NonPosScore = NonPosScore(-1000.0), // default is the negative of Score.PersonTotalScore
    defaultMaxTopicsPerSlot: Option[PosInt] = None,
    defaultMinPersonsPerTopic: PosInt = Constants.DefaultTopicMin,
    defaultMaxPersonsPerTopic: PosInt = Constants.DefaultTopicMax,
    unassigned: InputSettings.Unassigned = InputSettings.Unassigned(),
    absenceAdjustmentFactor: PosDouble = 0.8,
    statusDisplayInterval: FiniteDuration = 20.seconds
)

object InputSettings {

  /** Settings for unassigned topics. */
  final case class Unassigned(
      allowed: Boolean = false, // all other values in this class are unused when this is false
      minPersons: NonNegInt = 0, // O allow to not remove the topic, which let us skip a step when optimizing
      maxPersons: PosInt = Constants.IntLowMaxValue,
      personAntiPreference: NonPosScore = NonPosScore(-1000.0), // default is the negative of Score.PersonTotalScore
      personAntiPreferenceScaling: Option[InputSettings.UnassignedAntiPreferenceScaling] = None,
      personMultipleAntiPreference: Option[NonPosScore] = None
  )

  /** If this is enabled, the anti-preference for unassigned will scale with the number of forbidden topics for each person.
   * @param maximumAntiPreference The maximum value of the anti-preference (i.e., closest to zero).
   * @param forbiddenRatioForMaximum At this ratio of forbidden topics, the anti-preference will be up to its maximal value.
   */
  final case class UnassignedAntiPreferenceScaling(
      maximumAntiPreference: NonPosScore = NonPosScore(-1.0),
      forbiddenRatioForMaximum: Double = 0.75,
  )
}

final case class InputTableSettings(
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
    mandatoryPersonWeight: Weight = Weight.Default,
    forbiddenPersonMarker: Option[String] = None,
    preferencesScoreMapping: Option[Map[String, Score]] = None
)

final case class InputSlot(
    name: NonEmptyString,
    maxTopics: Option[PosInt] = None
)

final case class InputTopic(
    name: NonEmptyString,
    min: Option[PosInt] = None,
    max: Option[PosInt] = None,
    duration: Option[PosInt] = None,
    occurrences: Option[PosInt] = None,
    slots: Option[Set[NonEmptyString]] = None,
    presence: Option[Score] = None,
    forced: Boolean = false
) {

  /** Occurrence needs to be an Option to not appear when not needed */
  lazy val forcedOccurrences: PosInt = occurrences.getOrElse(1)

  /** Duration needs to be an Option to not appear when not needed */
  lazy val forcedDuration: PosInt = duration.getOrElse(1)

  lazy val occurrenceInstances: Seq[InputTopic.Occurrence] =
    if (forcedOccurrences == 1) Seq(InputTopic.Occurrence(this))
    else (1 to forcedOccurrences).map(InputTopic.Occurrence(this, _))

  if (forcedDuration > 2) throw new IllegalArgumentException("Currently, Gaston does not handle durations > 2")

}

object InputTopic {

  val OccurrenceMarker = "#"

  val PartMarker = "~"

  /** For multi-occurrence topic */
  final case class Occurrence(
      inputTopic: InputTopic,
      index: Opt[Int] = Opt.Missing // present only if there are multiple occurrences
  ) {
    lazy val name: String =
      index.fold(inputTopic.name)(i => s"${inputTopic.name} $OccurrenceMarker$i")

    /** Produce one part per duration unit of the topic */
    lazy val partInstances: Seq[InputTopic.Part] =
      if (inputTopic.forcedDuration == 1) Seq(Part(this))
      else (1 to inputTopic.forcedDuration).map(Part(this, _))
  }

  /** Fo multi-slot topics */
  final case class Part(
      occurrence: Occurrence,
      index: Opt[Int] = Opt.Missing // present only if there are multiple parts
  ) {
    lazy val name: String =
      index.fold(occurrence.name)(i => s"${occurrence.name} $PartMarker$i")
  }

}

final case class InputPerson(
    name: NonEmptyString,
    weight: Weight = Weight.Default,
    baseScore: Score = Score.Zero,
    absences: Set[NonEmptyString] = Set.empty,
    mandatory: Set[NonEmptyString] = Set.empty,
    forbidden: Set[NonEmptyString] = Set.empty,
    incompatible: Set[NonEmptyString] = Set.empty,
    wishes: Map[String, Score] = Map.empty, // can't use Refined as a key, see https://github.com/fthomas/refined/issues/443
    personWishes: Map[String, Score] = Map.empty,
    minFreeSlots: Option[PosInt] = None // how many free slots does this person want? None is zero..
)

final case class InputGlobalConstraints(
    simultaneous: Set[InputSimultaneousConstraint] = Set.empty,
    notSimultaneous: Set[InputSimultaneousConstraint] = Set.empty,
    exclusive: Set[InputExclusiveConstraint] = Set.empty,
    linked: Set[InputLinkedConstraint] = Set.empty,
)

final case class InputSimultaneousConstraint(
    topics: Set[NonEmptyString]
)

final case class InputExclusiveConstraint(
    topics: Set[NonEmptyString],
    inclusions: Option[Set[NonEmptyString]] = None,
    exemptions: Option[Set[NonEmptyString]] = None
) {

  lazy val forcedInclusions: Set[NonEmptyString] = inclusions.getOrElse(Set.empty)
  lazy val forcedExemptions: Set[NonEmptyString] = exemptions.getOrElse(Set.empty)

  if (inclusions.nonEmpty && exemptions.nonEmpty) {
    throw new IllegalArgumentException("Cannot have both inclusions and exemptions in exclusive constraint")
  }
}

final case class InputLinkedConstraint(
    topics: Set[NonEmptyString]
)

/* All necessary config-readers and config-writers */
given [A, C](using ConfigWriter[A]): ConfigWriter[IronType[A, C]] = ConfigWriter[A].contramap(identity)

given ConfigConvert[Score] = ConfigConvert[Double].xmap(Score.apply, _.value)
given ConfigReader[NonPosScore] = ConfigReader[Double :| Negative0].map(i => NonPosScore(i))
given ConfigReader[Weight] = ConfigReader[Double :| Positive].map(Weight.apply)
given ConfigWriter[Weight] = ConfigWriter[Double].contramap(_.value)

given ConfigConvert[InputLinkedConstraint] = deriveConvert[InputLinkedConstraint]
given ConfigConvert[InputExclusiveConstraint] = deriveConvert[InputExclusiveConstraint]
given ConfigConvert[InputSimultaneousConstraint] = deriveConvert[InputSimultaneousConstraint]
given ConfigConvert[InputGlobalConstraints] = deriveConvert[InputGlobalConstraints]
given ConfigConvert[InputPerson] = deriveConvert[InputPerson]
given ConfigConvert[InputTopic] = deriveConvert[InputTopic]
given ConfigConvert[InputSlot] = deriveConvert[InputSlot]
given ConfigConvert[InputTableSettings] = deriveConvert[InputTableSettings]
given ConfigConvert[InputSettings.UnassignedAntiPreferenceScaling] = deriveConvert[InputSettings.UnassignedAntiPreferenceScaling]
given ConfigConvert[InputSettings.Unassigned] = deriveConvert[InputSettings.Unassigned]
given ConfigConvert[InputSettings] = deriveConvert[InputSettings]
given ConfigConvert[InputModel] = deriveConvert[InputModel]
