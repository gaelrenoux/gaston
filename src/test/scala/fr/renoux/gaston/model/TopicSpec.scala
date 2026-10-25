package fr.renoux.gaston.model

import fr.renoux.gaston.MinimalTestModel.Slots.{Morning, Afternoon}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TopicSpec extends AnyFlatSpec with Matchers {

  behavior of "unassigned"

  it should "create a synthetic topic restricted to its slot" in {
    val topic = Topic.unassigned(42, Morning, max = 12)
    topic.id should be(42)
    topic.name should be("@Unassigned (morning)")
    topic.isUnassigned should be(true)
    topic.slots should be(Some(Set(Morning)))
    topic.requiresSingleSpecificSlot should be(true)
    topic.max should be(12)
  }

  it should "be forced when its min is zero" in {
    val topic = Topic.unassigned(42, Morning, min = 0)
    topic.min should be(0)
    topic.forced should be(true)
  }

  it should "be forced and have a min of zero when its min is one" in {
    val topic = Topic.unassigned(42, Morning, min = 1)
    topic.min should be(0)
    topic.forced should be(true)
  }

  it should "not be forced and keep its min when its min is more than one" in {
    val topic = Topic.unassigned(42, Morning, min = 3)
    topic.min should be(3)
    topic.forced should be(false)
  }

  behavior of "requiresSingleSpecificSlot"

  it should "be false for topics without slot restrictions or with several slots" in {
    Topic(0, "free").requiresSingleSpecificSlot should be(false)
    Topic(0, "two-slots", slots = Some(Set(Morning, Afternoon))).requiresSingleSpecificSlot should be(false)
  }

  it should "be true for topics with a single slot" in {
    Topic(0, "one-slot", slots = Some(Set(Morning))).requiresSingleSpecificSlot should be(true)
  }

}
