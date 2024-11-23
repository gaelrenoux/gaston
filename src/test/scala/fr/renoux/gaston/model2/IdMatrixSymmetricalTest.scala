package fr.renoux.gaston.model2

import fr.renoux.gaston.TestBase

class IdMatrixSymmetricalTest extends TestBase {
  val dimension = 3
  val countI: Count[SlotId] = dimension

  val testSeq: Seq[Seq[String]] = Seq(
    Seq(""),
    Seq("no", "no"),
    Seq("no", "yes", "yes")
  )

  "Creation" - {
    "tabulate" in {
      val matrix = IdMatrixSymmetrical.tabulate(countI) { (i, j) => testSeq(i.value)(j.value) }
      matrix.content.toSeq should be(testSeq.flatten)
    }

    "fill" in {
      val matrix = IdMatrixSymmetrical.fill(countI)("hello")
      val expected = Seq.fill(6)("hello")
      matrix.content.toSeq should be(expected)
    }

    "from" in {
      val matrix = IdMatrixSymmetrical.from[SlotId, String](testSeq)
      matrix.content.toSeq should be(testSeq.flatten)
    }
  }

  "toSeq" in {
    val matrix = IdMatrixSymmetrical.from[SlotId, String](testSeq)
    matrix.toSeq2(countI) should be(testSeq)
  }

  "apply" - {
    val matrix = IdMatrixSymmetrical.from[SlotId, String](testSeq)

    "inside the matrix's diagonal" in {
      matrix(0, 0) should be("")
      matrix(1, 1) should be("no")
      matrix(2, 2) should be("yes")
      for { i <- 0 until dimension } {
        matrix(i, i) should be(testSeq(i)(i))
      }
    }
    "inside the matrix's stored side" in {
      matrix(1, 0) should be("no")
      matrix(2, 1) should be("yes")
      for { i <- 0 until dimension; j <- 0 until i } {
        matrix(i, j) should be(testSeq(i)(j))
      }
    }
    "inside the matrix's non-stored side" in {
      matrix(0, 1) should be("no")
      matrix(1, 2) should be("yes")
      for { i <- 0 until dimension; j <- (i + 1) until dimension } {
        matrix(i, j) should be(testSeq(j)(i))
      }
    }
  }

  "update" - {

    "inside the matrix's diagonal" in {
      val matrix = IdMatrixSymmetrical.from[SlotId, String](testSeq)
      matrix(1, 1) = "hello"
      matrix(1, 1) should be("hello")
      for { i <- 0 until dimension; j <- 0 to i; if (i, j) != (1, 1) } {
        matrix(i, j) should be(testSeq(i)(j))
      }
      for { i <- 0 until dimension; j <- i + 1 until dimension } {
        matrix(i, j) should be(testSeq(j)(i))
      }
    }
    "inside the matrix's stored side" in {
      val matrix = IdMatrixSymmetrical.from[SlotId, String](testSeq)
      matrix(1, 0) = "hello"
      matrix(1, 0) should be("hello")
      matrix(0, 1) should be("hello")
      for { i <- 0 until dimension; j <- 0 to i; if (i, j) != (1, 0) } {
        matrix(i, j) should be(testSeq(i)(j))
      }
      for { i <- 0 until dimension; j <- i + 1 until dimension; if (i, j) != (0, 1) } {
        matrix(i, j) should be(testSeq(j)(i))
      }
    }
    "inside the matrix's non-stored side" in {
      val matrix = IdMatrixSymmetrical.from[SlotId, String](testSeq)
      matrix(1, 2) = "hello"
      matrix(1, 2) should be("hello")
      matrix(2, 1) should be("hello")
      for { i <- 0 until dimension; j <- 0 to i; if (i, j) != (2, 1) } {
        matrix(i, j) should be(testSeq(i)(j))
      }
      for { i <- 0 until dimension; j <- i + 1 until dimension; if (i, j) != (1, 2) } {
        matrix(i, j) should be(testSeq(j)(i))
      }
    }
  }

  "mapSumHalfToScore" in {
    val matrix = IdMatrixSymmetrical.from[SlotId, String](testSeq)
    val score = matrix.mapSumHalfToScore { (i, j, a) => i.value * 100 + j.value * 10 + a.length }(countI)
    score should be(0 + 102 + 112 + 202 + 213 + 223)
  }

}
