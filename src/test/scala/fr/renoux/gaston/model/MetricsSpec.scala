package fr.renoux.gaston.model

import fr.renoux.gaston.UdoConTestModel
import fr.renoux.gaston.model.problem.Metrics
import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}


class MetricsSpec extends FlatSpec with Matchers with PrivateMethodTester {

  val ComplexTestModel = fr.renoux.gaston.ComplexTestModel(42L)

  behavior of "upperLimit"

  it should "work on a very small number" in {
    Metrics.scheduleCountUpperLimit(slots = 2, topics = 3, minTopicsPerSlot = 1, maxTopicsPerSlot = 2) should be(12)
    //A-BC, B-AC, C-AB, AB-C, AC-B, BC-A, A-B, B-A, A-C, C-A, B-C, C-B
  }

  it should "work on a small number" in {
    Metrics.scheduleCountUpperLimit(slots = 2, topics = 4, minTopicsPerSlot = 1, maxTopicsPerSlot = 3) should be(50)
  }

  it should "work on a medium number" in {
    Metrics.scheduleCountUpperLimit(slots = 5, topics = 20, minTopicsPerSlot = 3, maxTopicsPerSlot = 4) should be(82353278007000L)
  }

  it should "work on a big number" in {
    val metrics = new Metrics(ComplexTestModel.Problems.Complete)
    metrics.scheduleCountUpperLimit should be(1867557041179830000L)
  }

  it should "work on a bigger number" in {
    val metrics = new Metrics(UdoConTestModel.Problems.Complete)
    metrics.scheduleCountUpperLimit should be(BigInt("346796529353273237958720"))

  }

}
