import sbt._

object Dependencies {
  
  object Version {
    val scalacheck = "1.15.4"
  }

  lazy val scalacheck: Seq[ModuleID] = Seq(
    "org.scalacheck" %% "scalacheck",
  ).map(_ % Version.scalacheck % Test withSources() withJavadoc())

}
