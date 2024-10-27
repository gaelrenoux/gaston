package fr.renoux.gaston.tools

import fr.renoux.gaston.TestUtils.*
import fr.renoux.gaston.input.{InputLoader, InputModel, InputTranscription}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class InputAnonymizerSpec extends AnyFlatSpec with Matchers {

  val uc2017: InputModel = InputLoader.fromClassPath("udocon2017/uc17-from-table.conf").force

  "Anonymizing UdoCon 2017" should "work" in {
    val anonymizer = new InputAnonymizer(uc2017)

    val initialProblem = InputTranscription.transcribe(uc2017).force
    val anonProblem = InputTranscription.transcribe(anonymizer.anonymized).force

    anonProblem.toAbstract should be(initialProblem.toAbstract)
  }

}
