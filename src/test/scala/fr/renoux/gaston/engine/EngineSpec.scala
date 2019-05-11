package fr.renoux.gaston.engine

import fr.renoux.gaston.SimpleTestModel
import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}

//import scala.util.Random


class EngineSpec extends FlatSpec with Matchers with PrivateMethodTester {
  val ComplexTestModel = fr.renoux.gaston.ComplexTestModel(42L)
  //private implicit val _r: Random = new Random(0L)

  assert(SimpleTestModel.Solutions.Best.isSolution)

  behavior of "run"
  //TODO some tests
}
