package fr.renoux.gaston.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RecordSpec extends AnyFlatSpec with Matchers {

  import fr.renoux.gaston.SimpleTestModel.Persons.*
  import fr.renoux.gaston.SimpleTestModel.Problems.*
  import fr.renoux.gaston.SimpleTestModel.Slots.*
  import fr.renoux.gaston.SimpleTestModel.Topics.*

  given Problem = WithUnassignedTopics

  /* Acting: mandatory Arthur, forbidden Bianca, from 2 to 5 persons */

  behavior of "Counts"

  it should "count required and open positions" in {
    val record = Record(Morning, Acting, Arthur)
    record.countPersons should be(1)
    record.countRequiredPositions should be(1)
    record.countOpenPositions should be(4)
    record.requiresMorePersons should be(true)
    record.canAddPersons should be(true)
    record.canRemovePersons should be(false)
  }

  it should "not have negative required positions" in {
    val record = Record(Morning, Acting, Arthur, Corwin, Daniela)
    record.countRequiredPositions should be(0)
    record.requiresMorePersons should be(false)
    record.canRemovePersons should be(true)
  }

  it should "not allow adding persons on a full record" in {
    val record = Record(Morning, Acting, Arthur, Corwin, Daniela, Fiona, Garion)
    record.countOpenPositions should be(0)
    record.canAddPersons should be(false)
  }

  it should "only consider non-mandatory persons as optional" in {
    Record(Morning, Acting, Arthur).optionalPersons should be(Set.empty)
    Record(Morning, Acting, Arthur, Corwin, Daniela).optionalPersons should be(Set(Corwin, Daniela))
  }

  it should "not allow removing persons when exactly at the min" in {
    Record(Morning, Acting, Arthur, Corwin).canRemovePersons should be(false)
  }

  behavior of "Modifications"

  it should "add, remove and replace persons" in {
    val record = Record(Morning, Acting, Arthur, Corwin)
    record.addPerson(Daniela) should be(Record(Morning, Acting, Arthur, Corwin, Daniela))
    record.removePerson(Corwin) should be(Record(Morning, Acting, Arthur))
    record.replacePerson(Corwin, Daniela) should be(Record(Morning, Acting, Arthur, Daniela))
  }

  it should "keep only mandatory persons when cleared" in {
    Record(Morning, Acting, Arthur, Corwin, Daniela).cleared should be(Record(Morning, Acting, Arthur))
  }

  behavior of "isSolution"

  it should "accept a correct record" in {
    val record = Record(Morning, Acting, Arthur, Corwin)
    record.isUnfilledSolution should be(true)
    record.isSolution should be(true)
    record.slowErrors should be(Nil)
  }

  it should "accept a record with too few persons as an unfilled solution only" in {
    val record = Record(Morning, Acting, Arthur)
    record.isUnfilledSolution should be(true)
    record.isSolution should be(false)
    record.slowErrors.size should be(1)
  }

  it should "reject a record with too many persons" in {
    val record = Record(Morning, Acting, Arthur, Corwin, Daniela, Fiona, Garion, Hercule)
    record.isUnfilledSolution should be(false)
    record.slowErrors.size should be(1)
  }

  it should "reject a record with a forbidden person" in {
    val record = Record(Morning, Acting, Arthur, Bianca)
    record.isUnfilledSolution should be(false)
    record.slowErrors.size should be(1)
  }

  it should "reject a record without its mandatory person" in {
    val record = Record(Morning, Acting, Corwin, Daniela)
    record.isUnfilledSolution should be(false)
    record.slowErrors.size should be(1)
  }

  it should "reject a record with an absent person" in {
    val record = Record(Morning, Acting, Arthur, Eric) // Eric is not there in the morning
    record.isUnfilledSolution should be(false)
    record.slowErrors.size should be(1)
  }

  it should "reject a record on a slot not allowed for its topic" in {
    val record = Record(Morning, UnassignedAfternoon, Corwin)
    record.isUnfilledSolution should be(false)
    record.slowErrors.size should be(1)
  }

  behavior of "Ordering"

  it should "order records by slot name, then by topic name" in {
    val records = List(
      Record(Morning, Bathing, Bianca),
      Record(AfterNoon, Cooking, Corwin),
      Record(Morning, Acting, Arthur),
    )
    records.sorted should be(List(
      Record(AfterNoon, Cooking, Corwin),
      Record(Morning, Acting, Arthur),
      Record(Morning, Bathing, Bianca),
    ))
  }

  behavior of "toFormattedString"

  it should "mark mandatory persons" in {
    Record(Morning, Acting, Arthur).toFormattedString should be("Acting ==> Arthur (MND)")
  }

}
