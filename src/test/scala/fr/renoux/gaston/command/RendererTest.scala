package fr.renoux.gaston.command

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class RendererTest extends AnyFlatSpec with Matchers {


  "ScoreDecimalFormat" should "work on negative numbers" in {
    Renderer.ScoreDecimalFormat.format(-15) should be("-015.00")
    Renderer.ShortScoreDecimalFormat.format(-15) should be("-015")
    Renderer.ScoreDecimalFormat.format(-150) should be("-150.00")
    Renderer.ShortScoreDecimalFormat.format(-150) should be("-150")
  }

  "ScoreDecimalFormat" should "work on positive numbers" in {
    Renderer.ScoreDecimalFormat.format(15) should be(" 015.00")
    Renderer.ShortScoreDecimalFormat.format(15) should be(" 015")
    Renderer.ScoreDecimalFormat.format(150) should be(" 150.00")
    Renderer.ShortScoreDecimalFormat.format(150) should be(" 150")
  }

}
