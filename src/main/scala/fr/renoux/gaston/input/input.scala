package fr.renoux.gaston

import java.nio.file.Path

import eu.timepit.refined.refineV
import fr.renoux.gaston.input.InputRefinements.{PosWeight, WeightPositive}
import fr.renoux.gaston.model._
import scalaz.{NonEmptyList, _}


package object input {

  type InputErrors = NonEmptyList[InputError]

  def InputErrors(hDesc: String, tDesc: String*): InputErrors = // scalastyle:ignore method.name
    NonEmptyList(InputError(hDesc), tDesc.map(InputError(_)): _*)

  /** Loads default values */
  def problemFromDefault: InputErrors \/ Problem = InputLoader.fromDefault.flatMap(transcribe)

  /** Loads from a specifically-named file if the classpath. */
  def problemFromClassPath(path: String): InputErrors \/ Problem = InputLoader.fromClassPath(path).flatMap(transcribe)

  /** Loads from defined files on the filesystem. */
  def problemFromPath(files: Path*): InputErrors \/ Problem = InputLoader.fromPath(files: _*).flatMap(transcribe)

  /** Loads from a String */
  def problemFromString(config: String): InputErrors \/ Problem = InputLoader.fromString(config).flatMap(transcribe)

  def transcribe(input: InputModel): InputErrors \/ Problem = new InputTranscription(input).result.disjunction

  val DefaultWeightRefined: PosWeight = refineV[WeightPositive](Weight.Default).getOrElse(throw new IllegalStateException(Weight.Default.toString))

}
