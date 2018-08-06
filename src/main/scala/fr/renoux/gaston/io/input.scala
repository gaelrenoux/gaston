package fr.renoux.gaston.io

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
                       preferences: Option[Set[InputPreference]]
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
                            strong: Option[Set[String]],
                            weak: Option[Set[String]]
                          )

case class InputPerson(
                        name: String,
                        weight: Option[Double],
                        incompatible: Option[Set[String]],
                        absences: Option[Set[String]]
                      )

case class InputTopic(
                       name: String,
                       mandatory: Option[Set[String]],
                       forbidden: Option[Set[String]],
                       linked: Option[Set[String]],
                       min: Option[Int],
                       max: Option[Int],
                       forcedSlot: Option[String]
                     )