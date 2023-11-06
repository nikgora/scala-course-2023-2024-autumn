import Dependencies._

lazy val root = project
  .in(file("."))
  .settings(name := "scala-course-2023-2024-autumn")
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= dependencies)
  .settings(testFrameworks += new TestFramework("org.scalacheck.ScalaCheckFramework"))
  .settings(Test / parallelExecution := false)

lazy val dependencies = scalacheck


lazy val commonSettings = Seq(
    scalaVersion := "3.1.0",
    organization := "karazin-scala-users-group",
    version      := "0.1.0-SNAPSHOT",
    scalacOptions ++= Seq(
        "-feature",
        "-deprecation",
        "-Xfatal-warnings"
    ),
    javacOptions ++= Seq(
        "-source", "11",
        "-target", "11"
    ),
)