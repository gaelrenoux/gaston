package fr.renoux.gaston.io

import java.nio.file.Path

import fr.renoux.gaston.Settings
import fr.renoux.gaston.model.problem.Problem

trait Input {

  def fromClassPath: (Problem, Settings)

  def from(files: Path*): (Problem, Settings)

}
