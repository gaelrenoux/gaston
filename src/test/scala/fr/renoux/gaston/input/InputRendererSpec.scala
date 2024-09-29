package fr.renoux.gaston.input

import fr.renoux.gaston.TestUtils._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class InputRendererSpec extends AnyFlatSpec with Matchers {

  "Rendering" should "correspond to the original" in {
    val input = InputLoader.fromClassPath("rendering-test.conf").force
    val rendered = InputRenderer.render(input)
    println("==============================")
    println(rendered)
    println("==============================")

    val source = Source.fromResource("rendering-test.conf").mkString
    rendered should be(source)
  }

}
