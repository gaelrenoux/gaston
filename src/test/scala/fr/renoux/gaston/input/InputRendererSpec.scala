package fr.renoux.gaston.input

import fr.renoux.gaston.TestUtils._
import fr.renoux.gaston.model._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

class InputRendererSpec extends AnyFlatSpec with Matchers {

  "Rendering" should "be nice" in {
    val input = InputLoader.fromClassPath("test-application.conf").force
    val rendered = InputRenderer.render(input)
    println(rendered)
  }

}
