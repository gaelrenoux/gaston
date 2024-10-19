package fr.renoux.gaston.tools

import fr.renoux.gaston.util.RandomImplicits.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.collection.Empty

import scala.collection.mutable
import scala.util.Random

/** Anonymize some names, replacing them by using a combination of two strings picked from two different lists.
  *
  * Class is mutable, and should be kept as long as you keep anonymizing the same model (as it prevents duplicates in
  * the names).
  *
  * Works through a series of passes. Whenever we are asked to anonymize a new name:
  * - We'll try getting Strings from the lists that where not used before (neither of them).
  * - Once all have been used, we'll try getting a combination that was not used before.
  * - Once all have been used, we'll start adding numbers at the end.
  *
  * We assume that the multiplying the size of all lists still lead to an Int.
  */
class CombinerAnonymizer(seed: Long, val altOneList: Seq[String], val altTwoList: Seq[String], prefix: String = "", suffix: String = "") {

  type NonEmptyString = String :| Not[Empty]

  private val altOneCount: Int = altOneList.size
  private val altTwoCount: Int = altTwoList.size

  /** Max number of combinations before we start putting it some numbers. */
  private val MaxBaseCombinations: Int = altOneCount * altTwoCount

  private val rand = new Random(seed)

  /** All anonymizations already decided. */
  private val anonymizations = mutable.Map[String, String]()

  private var firstPass = true
  private val altOneLeftsFirstPass: mutable.Set[Int] = mutable.Set.tabulate(altOneCount)(identity)
  private val altTwoLeftsFirstPass: mutable.Set[Int] = mutable.Set.tabulate(altTwoCount)(identity)

  private var secondPass = true
  private val combinationsLeftSecondPass: mutable.Set[Int] = mutable.Set.tabulate(MaxBaseCombinations)(identity)

  private lazy val numberingThirdPass: Array[Int] = Array.fill(MaxBaseCombinations)(2)

  def getCorrespondances: Map[String, String] = anonymizations.toMap

  def anonymizedNonEmpty(name: NonEmptyString): NonEmptyString = anonymized(name).refineUnsafe

  def anonymized(name: String): String = anonymizations.getOrElseUpdate(name, {
    createNameFirstPass() orElse createNameSecondPass() getOrElse createNameThirdPass()
  })

  private def createNameFirstPass(): Option[String] = if (!firstPass) None else Some {
    val ix1 = rand.pick(altOneLeftsFirstPass)
    altOneLeftsFirstPass -= ix1
    val ix2 = rand.pick(altTwoLeftsFirstPass)
    altTwoLeftsFirstPass -= ix2
    combinationsLeftSecondPass -= getKey(ix1, ix2)
    if (altOneLeftsFirstPass.isEmpty || altTwoLeftsFirstPass.isEmpty) {
      firstPass = false
    }
    getString(ix1, ix2)
  }

  private def createNameSecondPass(): Option[String] = if (!secondPass) None else Some {
    val key = rand.pick(combinationsLeftSecondPass)
    val (ix1, ix2) = fromKey(key)
    combinationsLeftSecondPass -= getKey(ix1, ix2)
    if (combinationsLeftSecondPass.isEmpty) {
      secondPass = false
    }
    getString(ix1, ix2)
  }

  private def createNameThirdPass(): String = {
    val key = rand.nextInt(MaxBaseCombinations)
    val (ix1, ix2) = fromKey(key)
    val number = numberingThirdPass(key)
    numberingThirdPass(key) = number + 1
    getString(ix1, ix2, number)
  }

  private def getString(altOneIndex: Int, altTwoIndex: Int) = {
    val altOne = altOneList(altOneIndex)
    val altTwo = altTwoList(altTwoIndex)
    s"$prefix $altOne $altTwo $suffix".trim
  }

  private def getString(altOneIndex: Int, altTwoIndex: Int, number: Int) = {
    val altOne = altOneList(altOneIndex)
    val altTwo = altTwoList(altTwoIndex)
    s"$prefix $altOne $altTwo $number $suffix"
  }

  private def getKey(givenName: Int, familyName: Int): Int = givenName * altTwoCount + familyName

  private def fromKey(key: Int): (Int, Int) = (key / altTwoCount, key % altTwoCount)

}
