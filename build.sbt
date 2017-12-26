lazy val commonSettings = Seq(
  name := "fp_in_scala",
  version := "0.1",
  scalaVersion := "2.12.0",

  libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.5" % "test"
)

scalaVersion := "2.12.0"

lazy val chapter2 = project.settings(commonSettings)
lazy val chapter3 = project.settings(commonSettings)
lazy val chapter4 = project.settings(commonSettings)
lazy val chapter5 = project.settings(commonSettings)
lazy val chapter6 = project.settings(commonSettings)
lazy val chapter7 = project.settings(commonSettings)
lazy val chapter9 = project.settings(commonSettings)
lazy val chapter10 = project.settings(commonSettings)
