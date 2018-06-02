package fr.renoux.gaston.runner

import java.text.DecimalFormat

import org.scalatest.{FlatSpec, Matchers}

class RunnerTest extends FlatSpec with Matchers {

  "Render" should "work" in {

    f"${14}%3d" should be (" 14")

    new DecimalFormat("###0.00").format(1.1456) should be ("1.15")
  }

}
