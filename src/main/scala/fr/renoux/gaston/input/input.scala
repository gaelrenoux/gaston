package fr.renoux.gaston

import java.nio.file.Path

import fr.renoux.gaston.model._
import scalaz.{NonEmptyList, _}


package object input {

  type InputErrors = NonEmptyList[InputError]

  def InputErrors(hDesc: String, tDesc: String*): InputErrors =
    NonEmptyList(InputError(hDesc), tDesc.map(InputError(_)): _*)

  /** Loads default values */
  def problemFromDefault: InputErrors \/ Problem = InputLoader.fromDefault.flatMap(transcribe)

  /** Loads from a specifically-named file if the classpath. */
  def problemFromClassPath(path: String): InputErrors \/ Problem = InputLoader.fromClassPath(path).flatMap(transcribe)

  /** Loads from defined files on the filesystem. */
  def problemFromPath(files: Path*): InputErrors \/ Problem = InputLoader.fromPath(files: _*).flatMap(transcribe)

  /** Loads from a String */
  def problemFromString(config: String): InputErrors \/ Problem = InputLoader.fromString(config).flatMap(transcribe)

  private def transcribe(input: InputModel): InputErrors \/ Problem = InputTranscription(input).problem.disjunction

}