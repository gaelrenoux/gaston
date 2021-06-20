package fr.renoux.gaston

import cats.data.NonEmptyList

import java.nio.file.Path
import eu.timepit.refined.refineV
import fr.renoux.gaston.input.InputRefinements.{PosWeight, WeightPositive}
import fr.renoux.gaston.model._


package object input {

  type InputErrors = NonEmptyList[InputError]

  def InputErrors(hDesc: String, tDesc: String*): InputErrors = // scalastyle:ignore method.name
    NonEmptyList.of(InputError(hDesc), tDesc.map(InputError(_)): _*)

  /** Loads default values */
  def problemFromDefault: Either[InputErrors, Problem] = InputLoader.fromDefault.flatMap(transcribe)

  /** Loads from a specifically-named file if the classpath. */
  def problemFromClassPath(path: String): Either[InputErrors, Problem] = InputLoader.fromClassPath(path).flatMap(transcribe)

  /** Loads from a file on the filesystem. */
  def problemFromPath(file: Path): Either[InputErrors, Problem] = InputLoader.fromPath(file).flatMap(transcribe)

  /** Loads from a String */
  def problemFromString(config: String): Either[InputErrors, Problem] = InputLoader.fromString(config).flatMap(transcribe)

  def transcribe(input: InputModel): Either[InputErrors, Problem] = new InputTranscription(input).result.toEither

  val DefaultWeightRefined: PosWeight = refineV[WeightPositive](Weight.Default).getOrElse(throw new IllegalStateException(Weight.Default.toString))

}
