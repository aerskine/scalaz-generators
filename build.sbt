organization := "com.github.aerskine"

name := "generators"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.0-M9" withSources(),
  "org.scalaz" %% "scalaz-effect" % "7.0.0-M9" withSources()
)
