package fr.renoux.gaston.engine

import fr.renoux.gaston.SimpleTestModel
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

//import scala.util.Random


class EngineSpec extends AnyFlatSpec with Matchers with PrivateMethodTester {
  val ComplexTestModel = fr.renoux.gaston.ComplexTestModel(42L)
  //private implicit val _r: Random = new Random(0L)

  assert(SimpleTestModel.Solutions.Best.isSolution)

  behavior of "run"
  //TODO some tests
}
