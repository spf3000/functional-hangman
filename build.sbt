import Dependencies._

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0-M4")

ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "alandevlin",
      scalaVersion := "2.12.8",
    )),
  name := "hangman",
  libraryDependencies += "org.scalaz" %% "scalaz-zio" % "1.0-RC1",
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.26",
  libraryDependencies += scalaTest    % Test
)
