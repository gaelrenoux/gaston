package fr.renoux.gaston.input

import fr.renoux.gaston.model._

/* TODO try without the Options, using default values */

/* All line and column indices are zero-based */

case class InputRoot(
    gaston: InputModel
)

case class InputModel(
    settings: InputSettings,
    udoSettings: Option[InputUdoSettings],
    slots: Set[String],
    persons: Set[InputPerson],
    topics: Set[InputTopic],
    preferences: Set[InputPreference] = Set()
)

case class InputSettings(
    weakPreference: Score,
    strongPreference: Score,
    incompatibilityAntiPreference: Score,
    defaultMin: Int,
    defaultMax: Int
)

case class InputUdoSettings(
    /* Column at which the persons start on the first line */
    personsStartingIndex: Int,
    /* Column for the topics */
    topicsIndex: Int,
    /* Column for the min number of persons on that topic */
    minPlayersIndex: Option[Int],
    /* Column for the max number of persons on that topic */
    maxPlayersIndex: Int,
    /* Weight given to any gamemaster */
    gamemasterWeight: Weight
)

case class InputPreference(
    person: String,
    strong: Set[String] = Set(),
    weak: Set[String] = Set()
)

case class InputPerson(
    name: String,
    weight: Double = Weight.Default.value,
    incompatible: Set[String] = Set(),
    absences: Set[String] = Set()
)

case class InputTopic(
    name: String,
    mandatory: Set[String] = Set(),
    forbidden: Set[String] = Set(),
    linked: Set[String] = Set(),
    min: Option[Int],
    max: Option[Int],
    forcedSlot: Option[String]
)