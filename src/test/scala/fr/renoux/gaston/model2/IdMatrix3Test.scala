package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase

class IdMatrix3Test extends TestBase {
  val depth = 2
  val height = 3
  val width = 4

  given countI: CountAll[SlotId] = CountAll[SlotId](depth)
  given countJ: CountAll[TopicId] = CountAll[TopicId](height)
  given countK: CountAll[PersonId] = CountAll[PersonId](width)

  val testSeq: Seq[Seq[Seq[String]]] = Seq(
    Seq(
      Seq("", "yes", "no", "yes"),
      Seq("no", "no", "no", ""),
      Seq("no", "no", "yes", "no")
    ),
    Seq(
      Seq("", "hello", "goodbye", "hello"),
      Seq("goodbye", "goodbye", "goodbye", ""),
      Seq("goodbye", "goodbye", "hello", "goodbye")
    )
  )

  "Creation" - {
    "tabulate" in {
      val matrix = IdMatrix3.tabulate[SlotId, TopicId, PersonId, String] { (i, j, k) => testSeq(i.value)(j.value)(k.value) }
      matrix.toSeq3 should be(testSeq)
    }

    "fill" in {
      val matrix = IdMatrix3.fill[SlotId, TopicId, PersonId, String]("hello")
      val expected = Seq.fill(depth, height, width)("hello")
      matrix.toSeq3 should be(expected)
    }

    "from" in {
      val matrix = IdMatrix3.unsafeFrom[SlotId, TopicId, PersonId, String](testSeq)
      matrix.toSeq3 should be(testSeq)
    }
  }

  "apply" - {
    "read key inside the matrix" in {
      val matrix = IdMatrix3.unsafeFrom[SlotId, TopicId, PersonId, String](testSeq)
      matrix(0, 0, 1) should be("yes")
      matrix(1, 1, 3) should be("")
      matrix(1, 2, 3) should be("goodbye")
      for { i <- 0 until depth; j <- 0 until height; k <- 0 until width } {
        matrix(i, j, k) should be(testSeq(i)(j)(k))
      }
    }

    "read key outside the matrix" in {
      val matrix = IdMatrix3.unsafeFrom[SlotId, TopicId, PersonId, String](testSeq)
      an[ArrayIndexOutOfBoundsException] shouldBe thrownBy { matrix(1, 3, 3) }
      an[ArrayIndexOutOfBoundsException] shouldBe thrownBy { matrix(-1, 1, 1) }
      // an[ArrayIndexOutOfBoundsException] shouldBe thrownBy { map(1, -1, 1) } // No, because it's flattened!
    }
  }

  "update" - {
    "update key inside the matrix" in {
      val matrix = IdMatrix3.unsafeFrom[SlotId, TopicId, PersonId, String](testSeq)
      matrix(0, 0, 1) = "no"
      matrix(1, 1, 3) = "hello"
      matrix(1, 2, 3) = ""
      matrix(0, 0, 1) should be("no")
      matrix(1, 1, 3) should be("hello")
      matrix(1, 2, 3) should be("")
      for { i <- 0 until depth; j <- 0 until height; k <- 0 until width } {
        if ((i, j, k) == (0, 0, 1)) matrix(i, j, k) should be("no")
        else if ((i, j, k) == (1, 1, 3)) matrix(i, j, k) should be("hello")
        else if ((i, j, k) == (1, 2, 3)) matrix(i, j, k) should be("")
        else matrix(i, j, k) should be(testSeq(i)(j)(k))
      }
    }
  }

}
