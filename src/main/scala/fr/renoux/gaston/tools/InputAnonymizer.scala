package fr.renoux.gaston.tools

import fr.renoux.gaston.input.InputModel
import fr.renoux.gaston.util.Resources.readNamesFile

import com.softwaremill.quicklens._

import scala.util.Random

/** Anonymize an input. Class can only be used once, re-instantiate for each usage. Mutable. */
class InputAnonymizer(in: InputModel, val seed: Long = 0) {

  // TODO Should move this to some utility
  given QuicklensFunctor[Set] with {
    override def map[A](as: Set[A])(f: A => A): Set[A] = as.map(f)
  }

  private val rand = new Random(seed)

  val personAnonymizer = new CombinerAnonymizer(
    rand.nextLong(), readNamesFile("persons1.txt"), readNamesFile("persons2.txt")
  )

  val topicAnonymizer = new CombinerAnonymizer(
    rand.nextLong(), readNamesFile("topics1.txt"), readNamesFile("topics2.txt"), "The"
  )

  val anonymized: InputModel = in
    .modify(_.topics.each.name).using(topicAnonymizer.anonymizedNonEmpty)
    .modify(_.persons.each.name).using(personAnonymizer.anonymizedNonEmpty)
    .modify(_.persons.each.mandatory.each).using(topicAnonymizer.anonymizedNonEmpty)
    .modify(_.persons.each.forbidden.each).using(topicAnonymizer.anonymizedNonEmpty)
    .modify(_.persons.each.incompatible.each).using(personAnonymizer.anonymizedNonEmpty)
    .modify(_.persons.each.wishes).using(_.map { case (key, value) => topicAnonymizer.anonymized(key) -> value })
    .modify(_.constraints.linked.each.topics.each).using(topicAnonymizer.anonymizedNonEmpty)
    .modify(_.constraints.exclusive.each.topics.each).using(topicAnonymizer.anonymizedNonEmpty)
    .modify(_.constraints.exclusive.each.exemptions.each).using(personAnonymizer.anonymizedNonEmpty)
    .modify(_.constraints.simultaneous.each.topics.each).using(topicAnonymizer.anonymizedNonEmpty)
    .modify(_.constraints.notSimultaneous.each.topics.each).using(topicAnonymizer.anonymizedNonEmpty)

  val anonymizedReordered: InputModel = anonymized
    .modify(_.topics).using(_.sortBy(_.name.value))
    .modify(_.persons).using(_.sortBy(_.name.value))

  /** Anonymize topics first, then persons. The hypothesis is that there won't be any topic name appearing in a person
    * name, but the reverse might be true. */
  def anonymizeTable(table: String): String = {
    val tableTopicsAnonymized = topicAnonymizer.getCorrespondances.foldLeft(table) {
      case (table, (oldTopic, newTopic)) => table.replace(oldTopic, newTopic)
    }
    val tablePersonsAnonymized = personAnonymizer.getCorrespondances.foldLeft(tableTopicsAnonymized) {
      case (table, (oldPerson, newPerson)) => table.replace(oldPerson, newPerson)
    }
    tablePersonsAnonymized
  }
}
