package fr.renoux.gaston.input

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InputCleanerSpec extends AnyFlatSpec with Matchers {

  // TODO Fix InputCleaner before restoring the Spec
  //  import InputCleaner.given
  //  import InputCleanerSpec.*
  //
  //  behavior of "Clean"
  //
  //  it should "ignore booleans" in {
  //    true.clean should be(true)
  //    false.clean should be(false)
  //  }
  //
  //  it should "ignore numbers" in {
  //    14.clean should be(14)
  //    -234L.clean should be(-234L)
  //  }
  //
  //  it should "clean Strings" in {
  //    "Hello ==> World, Earth".clean should be("Hello => World Earth")
  //  }
  //
  //  it should "clean String inside wrappers" in {
  //    Wrapper("Hello ==> World, Earth").clean should be(Wrapper("Hello => World Earth"))
  //  }
  //
  //  it should "clean Stuff" in {
  //    Stuff("Farewell ==> World, Earth", boolean = false).clean should be(Stuff("Farewell => World Earth", boolean = false))
  //  }
  //
  //  it should "clean MyInput" in {
  //    MyInput(
  //      42, "Hello ==> World, Earth",
  //      Thing(
  //        "Nothing to clean here",
  //        "Good afternoon ==> World, Earth",
  //        "Goodbye ==> World, Earth",
  //        Stuff("Farewell ==> World, Earth", boolean = false)
  //      )
  //    ).clean() should be(
  //      MyInput(
  //        42, "Hello => World Earth",
  //        Thing(
  //          "Nothing to clean here",
  //          "Good afternoon => World Earth",
  //          "Goodbye => World Earth",
  //          Stuff("Farewell => World Earth", boolean = false)
  //        )
  //      )
  //    )
  //  }
  //
  //  "Compilation" should "work on the actual input" in {
  //    val _ = implicitly[InputCleaner[InputModel]]
  //    InputLoader.fromDefault.clean() shouldNot be(InputModel()) // just testing compilation, we don't care about the result
  //  }

}

object InputCleanerSpec {

  final case class Wrapper(str: String) extends AnyVal

  final case class MyInput(
      int: Int,
      string: String,
      thing: Thing
  )

  final case class Thing(
      string: String,
      string2: String,
      string3: String,
      stuff: Stuff
  )

  final case class Stuff(
      string: String,
      boolean: Boolean
  )

}
