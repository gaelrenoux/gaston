package fr.renoux.gaston

import cats.data.NonEmptyList
import fr.renoux.gaston.model._
import fr.renoux.gaston.util.testOnly

/** In this package is everything that compiles the actual model we want to use from the inputs.
  *
  * As an aside, it also includes utilities to integrates the table representation of the preferences into the input.
  */
package object input {

  type InputErrors = NonEmptyList[InputError]

  def InputErrors(hDesc: String, tDesc: String*): InputErrors =
    NonEmptyList.of(InputError(hDesc), tDesc.map(InputError(_))*)

  /** Commodity method for tests: loads a problem from a specifically-named file if the classpath. */
  @testOnly
  def problemFromClassPath(path: String): Either[InputErrors, Problem] = InputLoader.fromClassPath(path).flatMap(InputTranscription.transcribe)

  /** Commodity method for tests: loads a problem from a specifically-named file if the classpath. */
  @testOnly
  def problemFromString(config: String): Either[InputErrors, Problem] = InputLoader.fromString(config).flatMap(InputTranscription.transcribe)

}
