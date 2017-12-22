package fr.renoux.gaston.io

import fr.renoux.gaston.model.Score
import org.scalatest.{FlatSpec, Matchers}

class UdoInputSpec extends FlatSpec with Matchers {

  behavior of "fromClassPath"
  it should "load the input from the ClassPath" in {
    val (problem, settings) = UdoInput.fromClassPath
    settings.weakPreference should be (Score(1))
    problem.persons.size should be >0
  }

}
