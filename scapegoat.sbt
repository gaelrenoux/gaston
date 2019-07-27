scapegoatVersion in ThisBuild := "1.3.8"

scapegoatDisabledInspections := Seq(
  "FinalModifierOnCaseClass",
  "MethodNames", "NullAssignment", "NullParameter", // done in scalastyle

  "TraversableHead", "OptionGet", "EitherGet" // for now
)

scapegoatIgnoredFiles := Seq()